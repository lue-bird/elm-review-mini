import * as path from "node:path"
import * as fs from "node:fs"
import * as os from "node:os"
import * as child_process from "node:child_process"
import * as readLine from "node:readline"

export interface ElmPorts {
    toJs: {
        subscribe: (callback: (fromElm: any) => void) => void
    }
    fromJs: { send: (toElm: any) => void }
}

export function compileElm(elmProjectDirectoryPath: string, mainElmModuleName: string[]): { init: () => { ports: ElmPorts } } {
    const elmJsPath = path.resolve(elmProjectDirectoryPath, "review-mini", "elm-stuff", "review-mini-cli-elm.js")
    child_process.execSync(
        `elm make ${path.join(...mainElmModuleName)} --output "${elmJsPath}"`, // TODO --optimize
        { cwd: elmProjectDirectoryPath }
    )
    eval(
        fs.readFileSync(elmJsPath, { encoding: "utf8" })
            .replace("(this)", "(globalThis)")
    )
    return (globalThis as any).Elm.Cli
}

export function programStart(elmPorts: ElmPorts) {
    function sendToElm(eventData: any): void {
        // console.log("js → elm: ", eventData)
        elmPorts.fromJs.send(eventData)
    }

    elmPorts.toJs.subscribe(async function (fromElm: { tag: string, value: any }) {
        // console.log("elm → js: ", fromElm)
        switch (fromElm.tag) {
            case "InitialFilesRequested": {

                const elmJsonSource = fs.readFileSync(path.resolve(process.cwd(), "elm.json"), { encoding: "utf8" })

                const elmJsonProject = JSON.parse(elmJsonSource)
                const directDependencyNameVersionStrings: string[] =
                    elmJsonProject["type"] === "package" ?
                        Object.entries(elmJsonProject["dependencies"])
                            .map(([name, versionConstraint]: [string, string]) =>
                                path.join(name, versionConstraint.substring(0, versionConstraint.indexOf(" ")))
                            )
                        : Object.entries(elmJsonProject["dependencies"]["direct"])
                            .map(([name, version]: [string, string]) =>
                                path.join(name, version)
                            )

                const directDependencies = await Promise.all(
                    directDependencyNameVersionStrings.map(packageDirectory =>
                        Promise.all([
                            fs.promises.readFile(path.resolve(elmHomePackages, packageDirectory, "elm.json"), { encoding: "utf8" }),
                            fs.promises.readFile(path.resolve(elmHomePackages, packageDirectory, "docs.json"), { encoding: "utf8" })
                        ])
                            .then(([elmJson, docsJson]) => ({
                                elmJson: JSON.parse(elmJson),
                                docsJson: JSON.parse(docsJson)
                            }))
                    )
                )

                const sourceDirectoryPaths: string[] =
                    elmJsonProject["type"] === "application" ?
                        elmJsonProject["source-directories"]
                            .map((sourceDirectoryPath: string) => path.resolve(process.cwd(), sourceDirectoryPath))
                        :
                        [path.resolve(process.cwd(), "src")]

                const modules: { path: string, source: string }[] = await Promise.all(
                    sourceDirectoryPaths
                        .map(directoryPath =>
                            fs.promises.readdir(directoryPath, { recursive: true, encoding: "utf8", withFileTypes: true })
                                .then(fileNames =>
                                    fileNames
                                        .filter(dirent => dirent.isFile())
                                        .map(dirent => path.resolve(directoryPath, dirent.path, dirent.name))
                                )
                        )
                ).then(filePaths =>
                    Promise.all(
                        filePaths
                            .flat()
                            .map(filePath =>
                                fs.promises.readFile(filePath, { encoding: "utf8" })
                                    .then(source => ({
                                        path: path.relative(process.cwd(), filePath),
                                        source: source
                                    }))
                            )
                    )
                )

                const extraPaths: string[] = fromElm.value.extraPaths
                const extraDirectoryPaths =
                    extraPaths
                        .map(directoryOrFilePath => path.resolve(process.cwd(), directoryOrFilePath))
                        .filter(path => fs.lstatSync(path).isDirectory())
                const extraFilePaths =
                    extraPaths
                        .map(directoryOrFilePath => path.resolve(process.cwd(), directoryOrFilePath))
                        .filter(path => fs.lstatSync(path).isFile())
                const extraFiles: { path: string, source: string }[] = await Promise.all(
                    extraDirectoryPaths
                        .map(directoryPath =>
                            fs.promises.readdir(directoryPath, { recursive: true, encoding: "utf8", withFileTypes: true })
                                .then(fileNames =>
                                    fileNames
                                        .filter(dirent => dirent.isFile())
                                        .map(dirent => path.resolve(directoryPath, dirent.path, dirent.name))
                                )
                        )
                ).then(filePaths =>
                    Promise.all(
                        filePaths
                            .flat()
                            .concat(extraFilePaths)
                            .map(filePath =>
                                fs.promises.readFile(filePath, { encoding: "utf8" })
                                    .then(source => ({
                                        path: path.relative(process.cwd(), filePath),
                                        source: source
                                    }))
                            )
                    )
                )
                sendToElm({
                    tag: "InitialFilesReceived",
                    value: {
                        elmJson: { source: elmJsonSource, project: elmJsonProject },
                        directDependencies: directDependencies,
                        modules: modules,
                        extraFiles: extraFiles
                    }
                })

                watchDirectories(
                    sourceDirectoryPaths,
                    {
                        onAddOrChange: async (fullPath) => {
                            sendToElm({
                                tag: "ModuleAddedOrChanged",
                                value: {
                                    path: path.relative(process.cwd(), fullPath),
                                    source: await fs.promises.readFile(fullPath, { encoding: "utf8" })
                                }
                            })
                        },
                        onDelete: async (fullPath) => {
                            sendToElm({ tag: "ModuleRemoved", value: { path: path.relative(process.cwd(), fullPath) } })
                        }
                    }
                )
                watchDirectories(
                    extraDirectoryPaths.concat(extraFilePaths),
                    {
                        onAddOrChange: async (fullPath) => {
                            sendToElm({
                                tag: "ExtraFileAddedOrChanged",
                                value: {
                                    path: path.relative(process.cwd(), fullPath),
                                    source: await fs.promises.readFile(fullPath, { encoding: "utf8" })
                                }
                            })
                        },
                        onDelete: async (fullPath) => {
                            sendToElm({ tag: "ExtraFileRemoved", value: { path: path.relative(process.cwd(), fullPath) } })
                        }
                    })
                break
            }
            case "ErrorsReceived": {
                console.log(fromElm.value.display)
                if (fromElm.value.fix !== null) {
                    const readLineInterface = readLine.promises.createInterface({
                        input: process.stdin,
                        output: process.stdout,
                    })
                    readLineInterface.question("To apply these edits, type y. To reject, type n")
                        .then(async (response) => {
                            if (response === "y") {
                                await Promise.all(
                                    fromElm.value.fix.fixedSources
                                        .map((fileFix: { path: string, source: string }) => {
                                            return fs.promises.writeFile(fileFix.path, fileFix.source)
                                        })
                                )
                                console.log("Ok. I applied the fix.")
                            } else {
                                console.log("Ok. I didn't change anything.")
                                sendToElm({ tag: "ErrorFixRejected", value: null })
                            }
                        })
                }
                break
            }
            case "JsonDecodingFailed": {
                console.log("an event from js to elm failed to decode: " + fromElm.value)
                break
            }
            default: {
                notifyOfUnknownMessageKind(fromElm.tag)
            }
        }
    })
}


function watchDirectories(directoryPaths: string[], onEvent: { onDelete: (fullPath: string) => Promise<void>, onAddOrChange: (fullPath: string) => Promise<void> }) {
    // most editors chunk up their file edits in 2, see
    // https://stackoverflow.com/questions/12978924/fs-watch-fired-twice-when-i-change-the-watched-file
    let debounced = true
    directoryPaths
        .forEach(directoryPath => {
            fs.watch(
                directoryPath,
                { recursive: true, encoding: "utf8" },
                async (_event, fileName) => {
                    if (debounced) {
                        debounced = false
                        if (fileName !== null) {
                            const currentTime = new Date()
                            console.log(`\n\n\n------ files changed at ${currentTime.getHours()}:${currentTime.getMinutes()}:${currentTime.getSeconds()}, reviewing again\n\n\n`)
                            console.clear()
                            const fullPath = path.join(directoryPath, fileName)
                            if (fs.existsSync(fullPath)) {
                                await onEvent.onAddOrChange(fullPath)
                            } else {
                                await onEvent.onDelete(fullPath)
                            }
                        }
                    } else {
                        setTimeout(() => { debounced = true }, 100)
                    }
                }
            )
        })
}


const elmRoot =
    process.env.ELM_HOME !== undefined ?
        process.env.ELM_HOME
        :
        path.join(
            os.homedir(),
            os.platform() === 'win32' ? 'AppData/Roaming/elm' : '.elm'
        )
const elmHomePackages =
    path.join(elmRoot, "0.19.1", "packages")

function notifyOfUnknownMessageKind(messageTag: string) {
    notifyOfBug("Unknown message kind " + messageTag + " from elm. The associated js implementation is missing")
}
function notifyOfBug(bugDescription: string) {
    console.error("bug: " + bugDescription + ". Please open an issue on github.com/lue-bird/elm-state-interface")
}
