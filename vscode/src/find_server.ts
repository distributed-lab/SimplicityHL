import * as cp from "child_process";
import * as fs from "fs";
import * as path from "path";
import * as os from "os";
import { env, ProgressLocation, Uri, window, workspace } from "vscode";
import process from "node:process";
import { spawn } from "node:child_process";

function findExecutable(command: string): string | null {
  try {
    const resolved = cp
      .execSync(
        process.platform === "win32" ? `where ${command}` : `which ${command}`,
      )
      .toString()
      .split(/\r?\n/)[0]
      .trim();
    if (resolved && fs.existsSync(resolved)) {
      return resolved;
    }
  } catch {
    // Not found in PATH
  }

  const commonDirs: string[] = [];

  if (process.platform === "win32") {
    commonDirs.push(
      path.join(
        process.env["USERPROFILE"] ?? "C:\\Users\\Default",
        ".cargo",
        "bin",
      ),
    );
  } else {
    commonDirs.push(path.join(os.homedir(), ".cargo", "bin"));

    commonDirs.push(
      "/usr/local/bin",
      "/usr/bin",
      path.join(os.homedir(), ".local", "bin"),
    );
  }

  for (const dir of commonDirs) {
    const candidate = path.join(dir, command);
    if (fs.existsSync(candidate)) {
      return candidate;
    }
  }

  return null;
}

async function installServer(command: string) {
  const cargoPath = findExecutable("cargo");
  if (!cargoPath) {
    throw new Error("Unable to find 'cargo'. Please ensure Rust is installed and in your PATH.");
  }
  const action = findExecutable(command) ? "Updating" : "Installing";


  return window.withProgress({
    location: ProgressLocation.Window,
    title: `${action} ${command}`,
    cancellable: false
  }, async (progress) => {
    return new Promise<void>((resolve, reject) => {
      const installProcess = spawn(cargoPath!, ["install", "--color", "never", command]);

      const reportProgress = (data: Buffer) => {
        const lines = data.toString()
          .split(/\r?\n/)
          .map(l => l.trim())

        for (const line of lines) {
          if (line.startsWith("Compiling") && line !== "Compiling") {
            progress.report({ message: line });
          }
        }
      };

      installProcess.stderr?.on('data', reportProgress);

      installProcess.on('close', (code) => {
        if (code === 0) {
          resolve();
        } else {
          reject(new Error(`Installation failed with exit code ${code}`));
        }
      });

      installProcess.on('error', (err) => {
        reject(new Error(`Failed to start cargo process: ${err.message}`));
      });
    });
  });
}


export async function ensureExecutable(
  command: string,
): Promise<string | null> {
  const cargoPath = findExecutable("cargo");
  const config = workspace.getConfiguration("simplicityhl");

  const suppressWarning = config.get<boolean>(
    "suppressMissingLspWarning",
    false,
  );

  if (!cargoPath && !suppressWarning) {
    const choice = await window.showWarningMessage(
      `To use SimplicityHL language server, please install cargo`,
      "Learn more",
      "Don't show again",
    );

    if (choice === "Learn more") {
      const url = "https://rust-lang.org/tools/install";
      await env.openExternal(Uri.parse(url));
    } else if (choice === "Don't show again") {
      const config = workspace.getConfiguration("simplicityhl");
      await config.update("suppressMissingLspWarning", true, true);
    }

    return null;
  }

  if (cargoPath) {
    try {
      await installServer(command);
      return findExecutable(command);
    } catch (err) {
      window.showErrorMessage(err);
      return null;
    }
  }
}
