#!usr/bin/env node

import * as path from "node:path"
import * as fs from "node:fs"
import * as os from "node:os"

export interface ElmPorts {
    toJs: {
        subscribe: (callback: (fromElm: any) => void) => void
    }
    fromJs: { send: (toElm: any) => void }
}

export function programStart(elmPorts: ElmPorts) {
    elmPorts.toJs.subscribe(function (fromElm: { tag: string, value: any }) {
        // console.log("elm → js: ", fromElm)
        function sendToElm(eventData: any): void {
            elmPorts.fromJs.send(eventData)
            // console.log("js → elm: ", eventData)
        }
        handleElmEvent(fromElm.tag, sendToElm)(fromElm.value)
    })
}

function handleElmEvent(tag: string, sendToElm: (v: any) => void): ((config: any) => void) {
    switch (tag) {
        case "ReadDependencies": return (config: string[]) => {
            sendToElm(
                config.map(packageDirectory => {
                    const fullPackagePath = path.join(elmHomePackages, packageDirectory)
                    return {
                        elmJson: fs.readFileSync(path.join(fullPackagePath, "elm.json")),
                        docsJson: fs.readFileSync(path.join(fullPackagePath, "docs.json"))
                    }
                })
            )
        }
        case "WatchElmFilesInDirectories": return (config: string[]) => {
            config.forEach(sourceDirectory => {
                fs.watch(
                    sourceDirectory,
                    { recursive: true },
                    (_event, fileName) => {
                        if ((fileName !== null) && fileName.endsWith(".elm")) {
                            // TODO depending on event
                            const fullFilePath = path.join(sourceDirectory, fileName)
                            if (fs.existsSync(fileName)) {
                                sendToElm({ tag: "AddedOrChanged", value: fs.readFileSync(fullFilePath) })
                            } else {
                                sendToElm({ tag: "Removed", value: fullFilePath })
                            }
                        }
                    }
                )
            })
        }
        default: return (_config: any) => {
            notifyOfUnknownMessageKind(tag)
        }
    }
}


const elmRoot =
    process.env.ELM_HOME
    || path.join(
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
