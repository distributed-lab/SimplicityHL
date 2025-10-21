import { LspClient } from "./client";
let client: LspClient;
export function activate() {
  client = new LspClient();
  void client.start();
}
export function deactivate(): Thenable<void> | undefined {
  if (!client) {
    return undefined;
  }
  return client.stop();
}
