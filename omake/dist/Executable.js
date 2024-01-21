"use strict";
var __createBinding = (this && this.__createBinding) || (Object.create ? (function(o, m, k, k2) {
    if (k2 === undefined) k2 = k;
    var desc = Object.getOwnPropertyDescriptor(m, k);
    if (!desc || ("get" in desc ? !m.__esModule : desc.writable || desc.configurable)) {
      desc = { enumerable: true, get: function() { return m[k]; } };
    }
    Object.defineProperty(o, k2, desc);
}) : (function(o, m, k, k2) {
    if (k2 === undefined) k2 = k;
    o[k2] = m[k];
}));
var __setModuleDefault = (this && this.__setModuleDefault) || (Object.create ? (function(o, v) {
    Object.defineProperty(o, "default", { enumerable: true, value: v });
}) : function(o, v) {
    o["default"] = v;
});
var __importStar = (this && this.__importStar) || function (mod) {
    if (mod && mod.__esModule) return mod;
    var result = {};
    if (mod != null) for (var k in mod) if (k !== "default" && Object.prototype.hasOwnProperty.call(mod, k)) __createBinding(result, mod, k);
    __setModuleDefault(result, mod);
    return result;
};
var __importDefault = (this && this.__importDefault) || function (mod) {
    return (mod && mod.__esModule) ? mod : { "default": mod };
};
Object.defineProperty(exports, "__esModule", { value: true });
exports.FileNotFoundException = exports.Executable = void 0;
const extrum_1 = require("@typinghare/extrum");
const child_process_1 = require("child_process");
const process_1 = __importDefault(require("process"));
const path_1 = __importDefault(require("path"));
const fs = __importStar(require("fs"));
/**
 * Executable.
 */
class Executable extends extrum_1.DataCollection {
    /**
     * Creates an executable.
     * @param env The environment where this executable will run.
     */
    constructor(env) {
        super({
            name: extrum_1.Datum.of(''),
            workingDir: extrum_1.Datum.of(''),
            files: extrum_1.Datum.of([]),
        });
        this.env = env;
    }
    /**
     * Adds all module files (.ml files) in the working directory.
     */
    addAllFilesInWorkingDir() {
        const workingDir = this.getValue('workingDir');
        const files = [];
        for (const file of workingDir) {
            const filePath = path_1.default.join(workingDir, file);
            const stat = fs.statSync(filePath);
            if (stat.isFile()) {
                files.push(filePath);
            }
        }
        this.getDatum('files').setValue(files);
    }
    /**
     * Runs this executable.
     */
    run() {
        // Convert files' relative paths to absolute paths
        const workingDir = this.env.getValue('workingDir');
        const absoluteWorkingDir = path_1.default.join(workingDir, this.getValue('workingDir'));
        const getAbsoluteFilepath = (file) => path_1.default.join(absoluteWorkingDir, file).trim();
        const files = this.getValue('files').map(getAbsoluteFilepath);
        // Check if all files exist
        files.forEach(this.checkFile);
        // Compile files
        const outputDir = this.env.getValue('outputDir');
        const targetFilepath = path_1.default.join(outputDir, this.getValue('name'));
        const command = `ocamlc -o ${targetFilepath} ${files.join(' ')}`;
        (0, child_process_1.exec)(command, (error, stdout, stderr) => {
            if (error && this.checkExternalError(error, stdout, stderr)) {
                return;
            }
            // Execute the built file
            this.executeFile(targetFilepath);
            // Remove all intermediate compiled files
            this.removeIntermediateFiles(files);
        });
    }
    /**
     * Checks whether file exists.
     * @param file The file to check.
     * @private
     */
    checkFile(file) {
        if (!fs.existsSync(file)) {
            throw new FileNotFoundException(file);
        }
    }
    /**
     * Logs error information.
     * @param error The error encountered.
     * @param stderr The standard error output string.
     * @private
     */
    logError(error, stderr) {
        if (error) {
            console.log(`An internal exception is encountered: `);
            console.error(error.message);
            console.log(`\nError output:`);
            console.error(stderr);
            return;
        }
    }
    /**
     * Executes an executable file.
     * @param targetFilepath The path of the executable file.
     * @private
     */
    executeFile(targetFilepath) {
        (0, child_process_1.exec)(targetFilepath, (error, stdout, stderr) => {
            if (error && this.checkExternalError(error, stdout, stderr)) {
                return;
            }
            // This prevents the trailing '%' of the output
            if (!stdout.endsWith('\n')) {
                stdout += '\n';
            }
            process_1.default.stdout.write(stdout, 'utf-8');
        });
    }
    /**
     * Removes .cmi(compiled interface file) and .cmo(compiled object file).
     * @private
     */
    removeIntermediateFiles(files) {
        const dirSet = new Set();
        files.forEach((file) => dirSet.add(path_1.default.dirname(file)));
        const dirString = Array.from(dirSet).join(' ');
        (0, child_process_1.exec)(`find ${dirString} -name "*.cmi" -delete`);
        (0, child_process_1.exec)(`find ${dirString} -name "*.cmo" -delete`);
    }
    /**
     * Logs an external error (if exists)
     * @param error The error encountered.
     * @param stdout The standard output string.
     * @param stderr The standard error output string.
     * @return false if the external error exists; true otherwise.
     * @private
     */
    checkExternalError(error, stdout, stderr) {
        if (error) {
            this.logError(error, stderr);
            if (error.code !== 0) {
                console.log(`Fail to execute [ ${this.getValue('name')} ]: `);
                console.log(stdout);
            }
            return true;
        }
        return false;
    }
}
exports.Executable = Executable;
class FileNotFoundException extends Error {
    constructor(filepath) {
        super(`File not found: ${filepath}`);
    }
}
exports.FileNotFoundException = FileNotFoundException;
