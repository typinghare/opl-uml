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
Object.defineProperty(exports, "__esModule", { value: true });
exports.ExecutableManager = exports.Executable = exports.Environment = void 0;
const path = __importStar(require("path"));
const child_process_1 = require("child_process");
const extrum_1 = require("@typinghare/extrum");
/**
 * The environment.
 */
class Environment {
    /**
     * Creates an environment.
     * @param workingDir The working directory.
     * @param outputDir The output directory.
     */
    constructor(workingDir, outputDir) {
        this.workingDir = workingDir;
        this.outputDir = outputDir;
    }
}
exports.Environment = Environment;
/**
 * An OCaml executable
 */
class Executable extends extrum_1.DataCollection {
    /**
     * Creates an executable.
     * @param env The environment.
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
     * Runs this executable.
     */
    run() {
        // Files
        const absoluteWorkingDir = path.join(this.env.workingDir, this.getValue('workingDir'));
        const filepathMapper = (file) => path.join(absoluteWorkingDir, file);
        const files = this.getValue('files').map(filepathMapper);
        // Compile files
        const targetFilepath = path.join(this.env.outputDir, this.getValue('name'));
        const command = `ocamlc -o ${targetFilepath} ${files.join(' ')}`;
        (0, child_process_1.exec)(command, () => {
            this.executeFile(targetFilepath);
            this.removeIntermediateFiles(files);
        });
    }
    /**
     * Executes an executable file.
     * @param targetFilepath The path of the executable file.
     * @private
     */
    executeFile(targetFilepath) {
        (0, child_process_1.exec)(targetFilepath, (error, stdout, stderr) => {
            if (error) {
                console.error(`exec error: ${error}`);
                console.error(stderr);
                return;
            }
            process.stdout.write(stdout);
        });
    }
    /**
     * Removes .cmi(compiled interface file) and .cmo(compiled object file).
     * @private
     */
    removeIntermediateFiles(files) {
        const dirSet = new Set();
        files.forEach((file) => dirSet.add(path.dirname(file)));
        const dirString = Array.from(dirSet).join(' ');
        (0, child_process_1.exec)(`find ${dirString} -name "*.cmi" -delete`);
        (0, child_process_1.exec)(`find ${dirString} -name "*.cmo" -delete`);
    }
}
exports.Executable = Executable;
/**
 * Executable manager.
 */
class ExecutableManager {
    /**
     * Creates an executable manager.
     * @param env The environment.
     */
    constructor(env) {
        this.env = env;
        this.byName = new Map();
    }
    /**
     * Adds an executable.
     * @param name The name of the executable (must be unique).
     * @param workingDir The working directory.
     * @param files The list of files to compile.
     */
    add(name, workingDir, files) {
        if (this.byName.has(name)) {
            throw new Error(`Duplicate executable name: ${name}`);
        }
        // Default file: main.ml
        if (files.length === 0) {
            files.push('main.ml');
        }
        const executable = new Executable(this.env);
        executable.getDatum('name').setValue(name);
        executable.getDatum('workingDir').setValue(workingDir);
        executable.getDatum('files').setValue(files);
        this.byName.set(name, executable);
    }
    /**
     * Returns all executables.
     */
    getExecutables() {
        return Array.from(this.byName.values());
    }
}
exports.ExecutableManager = ExecutableManager;
