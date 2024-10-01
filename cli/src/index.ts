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

/** Small helper for creating a runnable elm program. Requires the `elm` binary to be available.
 * Feel free to use a custom loader that e.g. allows debug or uses an alternative compiler.
 * 
 * @param elmProjectDirectoryPath The path to the `review-mini/` elm application. Use `import.meta.dirname`
 * @param mainElmModuleNamePath 
 * @returns An elm worker program you can start whenever you want which then provides its ports
 */
export function compileElm(elmProjectDirectoryPath: string, mainElmModuleNamePath: string): { init: () => { ports: ElmPorts } } {
    if (elmProjectDirectoryPath === undefined) {
        console.error("To access import.meta.dirname you'll need at least Node.js 20.11 / 21.2: https://stackoverflow.com/a/50052194")
        throw new Error()
    }
    const elmJsPath = path.join(elmProjectDirectoryPath, "elm-stuff", "temp-review-mini-cli-elm.js")
    child_process.execSync(
        // if you want to use Debug.log etc while prototyping or debugging, remove the --optimize flag
        // if you need extra performance switch to https://github.com/mdgriffith/elm-optimize-level-2
        `elm make ${mainElmModuleNamePath} --output "${elmJsPath}" --optimize`,
        { cwd: elmProjectDirectoryPath }
    )
    eval(
        fs.readFileSync(elmJsPath, { encoding: "utf8" })
            .replace("(this)", "(globalThis)")
    )
    fs.unlinkSync(elmJsPath)
    return (globalThis as any).Elm.Cli
}

export function startWatching(elmPorts: ElmPorts) {
    process.on("SIGINT", () => {
        process.exit()
    })
    function sendToElm(eventData: any): void {
        // console.log("js → elm: ", eventData)
        elmPorts.fromJs.send(eventData)
    }

    elmPorts.toJs.subscribe(async function (fromElm: { tag: string, value: any }) {
        // console.log("elm → js: ", fromElm)
        switch (fromElm.tag) {
            case "InitialFilesRequested": {
                const elmJsonSource = await fs.promises.readFile(
                    path.resolve(process.cwd(), "elm.json"),
                    { encoding: "utf8" }
                )
                const elmJsonProject = JSON.parse(elmJsonSource)

                const sourceDirectoryPaths: string[] =
                    elmJsonSourceDirectoryRelativePaths(elmJsonProject)
                        .map(relative => path.resolve(process.cwd(), relative))

                const modules: { path: string, source: string }[] =
                    await Promise.all(
                        sourceDirectoryPaths
                            .map(directoryPath => readAllContainedFilePaths(directoryPath))
                    )
                        .then(filePaths =>
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

                const extraPaths: string[] =
                    (fromElm.value.extraPaths as string[])
                        .map(directoryOrFilePath => path.resolve(process.cwd(), directoryOrFilePath))
                        .filter(path => {
                            if (fs.existsSync(path)) {
                                return true
                            } else {
                                console.warn(`The configured extra path ${path} does not exist. Proceeding without inspecting this path`)
                                return false
                            }
                        })
                const extraDirectoryPaths =
                    extraPaths.filter(path => fs.lstatSync(path).isDirectory())
                const extraFilePaths =
                    extraPaths.filter(path => fs.lstatSync(path).isFile())

                const allExtraFiles: { path: string, source: string }[] =
                    await Promise.all(
                        extraDirectoryPaths
                            .map(directoryPath => readAllContainedFilePaths(directoryPath))
                    )
                        .then(filePaths =>
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
                        directDependencies: await readDirectDependencies(elmJsonProject),
                        modules: modules,
                        extraFiles: allExtraFiles
                    }
                })

                watchPaths({
                    paths: sourceDirectoryPaths,
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
                        sendToElm({
                            tag: "ModuleRemoved",
                            value: { path: path.relative(process.cwd(), fullPath) }
                        })
                    }
                })
                watchPaths({
                    paths: extraPaths,
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
                        sendToElm({
                            tag: "ExtraFileRemoved",
                            value: { path: path.relative(process.cwd(), fullPath) }
                        })
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
                        // https://github.com/nodejs/node/issues/30510
                        terminal: false
                    })
                    readLineInterface.question("To apply these edits, enter y. To reject, enter n: ")
                        .then(async (response) => {
                            readLineInterface.close()
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
            case "ProblemEncountered": {
                console.log(fromElm.value)
                break
            }
            default: {
                console.error(`bug: unknown message kind ${fromElm.tag} from elm. The associated js implementation is missing. Please open an issue on github.com/lue-bird/elm-review-mini`)
            }
        }
    })
}

function elmJsonSourceDirectoryRelativePaths(elmJsonProject: any): string[] {
    return elmJsonProject["type"] === "application" ?
        elmJsonProject["source-directories"]
        : // elmJsonProject["type"] === "package"
        ["src"]
}

function readDirectDependencies(elmJsonProject: any): Promise<{ elmJson: any, docsJson: any }[]> {
    const directDependencyPaths: string[] =
        elmJsonProject["type"] === "package" ?
            Object.entries(elmJsonProject["dependencies"])
                .map(([name, versionConstraint]: [string, string]) =>
                    path.join(elmHomePackages, name, versionConstraint.substring(0, versionConstraint.indexOf(" ")))
                )
            : Object.entries(elmJsonProject["dependencies"]["direct"])
                .map(([name, version]: [string, string]) =>
                    path.join(elmHomePackages, name, version)
                )
    return Promise.all(
        directDependencyPaths
            .map(packageDirectory =>
                Promise.all([
                    fs.promises.readFile(
                        path.resolve(packageDirectory, "elm.json"),
                        { encoding: "utf8" }
                    ),
                    fs.promises.readFile(
                        path.resolve(packageDirectory, "docs.json"),
                        { encoding: "utf8" }
                    )
                ])
                    .then(([elmJson, docsJson]) => ({
                        elmJson: JSON.parse(elmJson),
                        docsJson: JSON.parse(docsJson)
                    }))
            )
    )
}

function readAllContainedFilePaths(directoryPath: string): Promise<string[]> {
    return fs.promises.readdir(directoryPath, { recursive: true, encoding: "utf8", withFileTypes: true })
        .then(fileNames =>
            fileNames
                .filter(dirent => dirent.isFile())
                .map(dirent => path.resolve(directoryPath, dirent.parentPath, dirent.name))
        )
}


function watchPaths(
    watch: {
        paths: string[],
        onDelete: (fullPath: string) => Promise<void>,
        onAddOrChange: (fullPath: string) => Promise<void>
    }
) {
    // most editors chunk up their file edits in 2, see
    // https://stackoverflow.com/questions/12978924/fs-watch-fired-twice-when-i-change-the-watched-file
    let debounced = true
    watch.paths
        .forEach(directoryOrFilePath => {
            fs.watch(
                directoryOrFilePath,
                { recursive: true, encoding: "utf8" },
                async (_event, fileName) => {
                    if (debounced) {
                        debounced = false
                        if (fileName !== null) {
                            const currentTime = new Date()
                            console.log(`\n\n\n------ files changed at ${currentTime.getHours()}:${currentTime.getMinutes()}:${currentTime.getSeconds()}, reviewing again\n\n\n`)
                            console.clear()
                            const fullPath = path.join(directoryOrFilePath, fileName)
                            if (fs.existsSync(fullPath)) {
                                await watch.onAddOrChange(fullPath)
                            } else {
                                await watch.onDelete(fullPath)
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
