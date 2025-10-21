import { window } from "vscode";
import {
  Executable,
  LanguageClient,
  LanguageClientOptions,
  ServerOptions,
} from "vscode-languageclient/node";
import { ensureExecutable } from "./find_server";
import process from "node:process";

export class LspClient {
  private client: LanguageClient | undefined;

  public async start(): Promise<void> {
    const command = "simplicityhl-lsp";
    const execPath = await ensureExecutable(command);

    if (!execPath) {
      return;
    }

    const run: Executable = {
      command: execPath,
      options: {
        env: {
          ...process.env,
        },
      },
    };
    const serverOptions: ServerOptions = {
      run,
      debug: run,
    };

    const clientOptions: LanguageClientOptions = {
      documentSelector: [{ scheme: "file", language: "simplicityhl" }],
    };

    this.client = new LanguageClient(
      "simplicityhlLspClient",
      "SimplicityHL LSP",
      serverOptions,
      clientOptions,
    );

    try {
      await this.client.start();
      window.showInformationMessage("SimplicityHL Language Server activated!");
    } catch (e) {
      window.showErrorMessage(
        `Failed to start SimplicityHL Language Server: ${e}`,
      );
    }
  }

  public stop(): Thenable<void> | undefined {
    if (!this.client) {
      return undefined;
    }
    return this.client.stop();
  }
}
