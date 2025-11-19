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

  public async stop(): Promise<void> {
    if (!this.client) {
      return;
    }
    await this.client.stop();
    this.client = undefined;
  }

  public async restart(): Promise<void> {
    if (!this.client) {
      window.showWarningMessage("LSP client not initialized. Cannot restart.");
      return;
    }

    try {
      await this.stop();
      await this.start();
      window.showInformationMessage("SimplicityHL Language Server restarted successfully!");
    } catch (e) {
      window.showErrorMessage(`Failed to restart LSP: ${e}`);
    }
  }
}
