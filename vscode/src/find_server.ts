import * as cp from "child_process";
import * as fs from "fs";
import * as path from "path";
import * as os from "os";
import { env, Uri, window, workspace } from "vscode";

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

export async function ensureExecutable(
  command: string,
): Promise<string | null> {
  const exePath = findExecutable(command);
  const config = workspace.getConfiguration("simplicityhl");

  const suppressWarning = config.get<boolean>(
    "suppressMissingLspWarning",
    false,
  );
  if (!exePath && !suppressWarning) {
    const choice = await window.showWarningMessage(
      `LSP server "${command}" was not found in PATH or common locations. To use language server feautures, please install server to PATH`,
      "Learn more",
      "Don't show again",
      "Close",
    );

    if (choice === "Learn more") {
      const url = "https://github.com/distributed-lab/simplicityhl-lsp";
      await env.openExternal(Uri.parse(url));
    } else if (choice === "Don't show again") {
      const config = workspace.getConfiguration("simplicityhl");
      await config.update("suppressMissingLspWarning", true, true);
    }

    return null;
  }
  return exePath;
}
