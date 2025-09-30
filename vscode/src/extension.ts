import { ExtensionContext, window, workspace } from "vscode";
import {
  Executable,
  LanguageClient,
  LanguageClientOptions,
  ServerOptions,
} from "vscode-languageclient/node";
import * as cp from "child_process";
import { ensureExecutable } from "./find_server";
let client: LanguageClient;
export async function activate(context: ExtensionContext) {
  const command = "simplicityhl-lsp";
  const exec = await ensureExecutable(command);
  if (!exec) {
    return;
  }
  window.showInformationMessage("SimplicityHL Language Server activated!");
  const run: Executable = {
    command,
    options: {
      env: {
        ...process.env,
        RUST_LOG: "debug",
      },
    },
  };
  // If the extension is launched in debug mode then the debug server options are used
  // Otherwise the run options are used
  const serverOptions: ServerOptions = {
    run,
    debug: run,
  };
  // Options to control the language client
  const clientOptions: LanguageClientOptions = {
    documentSelector: [{ scheme: "file", language: "simplicityhl" }],
  };
  // Create the language client and start the client.
  client = new LanguageClient(
    exec,
    "SimplicityHL LSP",
    serverOptions,
    clientOptions,
  );
  // Start the client. This will also launch the server
  client.start();
}
export function deactivate(): Thenable<void> | undefined {
  if (!client) {
    return undefined;
  }
  return client.stop();
}
