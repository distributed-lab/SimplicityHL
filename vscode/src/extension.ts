import { LspClient } from "./client";
import { registerRestartCommand } from "./commands"
import { ExtensionContext } from "vscode"

let client: LspClient;

export function activate(context: ExtensionContext) {
  client = new LspClient();
  void client.start();

  registerRestartCommand(context, client);
}
export function deactivate(): Thenable<void> | undefined {
  if (!client) {
    return undefined;
  }
  return client.stop();
}
