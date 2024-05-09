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

export function compileElm(elmProjectDirectoryPath: string): { init: () => { ports: ElmPorts } } {
    const elmJsPath = path.resolve(elmProjectDirectoryPath, "review-mini", "elm-stuff", "review-mini-cli-elm.js")
    child_process.execSync(
        `elm make --optimize src/Cli.elm --output "${elmJsPath}"`,
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

    elmPorts.toJs.subscribe(function (fromElm: { tag: string, value: any }) {
        // console.log("elm → js: ", fromElm)
        switch (fromElm.tag) {
            case "InitialFilesRequested": {
                const extraPaths: string[] = fromElm.value.extraPaths

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

                const directDependencies =
                    directDependencyNameVersionStrings.map(packageDirectory => ({
                        elmJson: fs.readFileSync(path.resolve(elmHomePackages, packageDirectory, "elm.json"), { encoding: "utf8" }),
                        docsJson: fs.readFileSync(path.resolve(elmHomePackages, packageDirectory, "docs.json"), { encoding: "utf8" })
                    }))


                const sourceDirectoryPaths: string[] =
                    elmJsonProject["type"] === "application" ?
                        elmJsonProject["source-directories"]
                            .map((sourceDirectoryPath: string) => path.resolve(process.cwd(), sourceDirectoryPath))
                        :
                        [path.resolve(process.cwd(), "src")]

                const extraDirectoryPaths =
                    extraPaths
                        .map(directoryOrFilePath => path.resolve(process.cwd(), directoryOrFilePath))
                        .filter(path => fs.lstatSync(path).isDirectory())
                const extraFilePaths =
                    extraPaths
                        .map(directoryOrFilePath => path.resolve(process.cwd(), directoryOrFilePath))
                        .filter(path => fs.lstatSync(path).isFile())

                const files: { path: string, source: string }[] =
                    [...sourceDirectoryPaths, ...extraDirectoryPaths]
                        .flatMap(directoryPath =>
                            fs.readdirSync(directoryPath, { recursive: true, encoding: "utf8" })
                                .map(fileName => path.resolve(directoryPath, fileName))
                                .filter(path => fs.lstatSync(path).isFile())
                        )
                        .concat(extraFilePaths)
                        .map(filePath => ({
                            path: filePath,
                            source: fs.readFileSync(filePath, { encoding: "utf8" })
                        }));

                sendToElm({
                    tag: "InitialFilesReceived",
                    value: {
                        elmJson: { source: elmJsonSource, project: elmJsonProject },
                        directDependencies: directDependencies,
                        files: files
                    }
                });

                [...sourceDirectoryPaths, ...extraDirectoryPaths, ...extraFilePaths]
                    .forEach(absoluteDirectoryOrFilePath => {
                        fs.watch(
                            absoluteDirectoryOrFilePath,
                            { recursive: true, encoding: "utf8" },
                            (_event, fileName) => {
                                if (fileName !== null) {
                                    const fullPath = path.join(absoluteDirectoryOrFilePath, fileName)
                                    if (fs.existsSync(fullPath)) {
                                        sendToElm({
                                            tag: "FileAddedOrChanged",
                                            value: {
                                                path: fullPath,
                                                source: fs.readFileSync(fullPath, { encoding: "utf8" })
                                            }
                                        })
                                    } else {
                                        sendToElm({ tag: "FileRemoved", value: { path: fullPath } })
                                    }
                                }
                            }
                        )
                    })
                break
            }
            case "ErrorsReceived": {
                // wait for user yes/no and sendToElm
                const readLineInterface = readLine.promises.createInterface({
                    input: process.stdin,
                    output: process.stdout,
                })
                console.log(fromElm.value.display)
                if (fromElm.value.fix !== null) {
                    readLineInterface.question("apply → y, reject → n")
                        .then(response => {
                            if (response === "y") {
                                fs.writeFileSync(fromElm.value.fix.path, fromElm.value.fix.fixedSource)
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
