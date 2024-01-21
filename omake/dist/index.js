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
const process = __importStar(require("process"));
const Environment_1 = require("./Environment");
const path_1 = __importDefault(require("path"));
const Application_1 = require("./Application");
const fs_1 = __importDefault(require("fs"));
const ExecutableManager_1 = require("./ExecutableManager");
const os_1 = require("os");
// Global constants
const OUTPUT_DIR_PATH = 'target';
const CONFIG_PATH = 'OMake.ini';
const CONFIG_ENCODING = 'utf-8';
// Helper functions associated with .ini configuration file
const IniFn = {
    isNotComment: (line) => !line.startsWith('#'),
    breakLine: (line) => line.split('='),
    isValidLine: (sp) => sp.length == 2,
    trimLine: (line) => line.trim(),
};
const currentWorkingDir = process.cwd();
const executableName = process.argv[2];
// Set up environment
const env = new Environment_1.Environment();
const workingDir = path_1.default.join(currentWorkingDir);
const outputDir = path_1.default.join(currentWorkingDir, OUTPUT_DIR_PATH);
env.getDatum('workingDir').setValue(workingDir);
env.getDatum('outputDir').setValue(outputDir);
// Create the output directory if it does not exist
if (!fs_1.default.existsSync(outputDir)) {
    fs_1.default.mkdirSync(outputDir);
}
// Create an application
const application = new Application_1.Application(env);
const executableManger = application.getManager(ExecutableManager_1.ExecutableManager);
// Load configuration file
const configurationPath = path_1.default.join(workingDir, CONFIG_PATH);
const configLines = fs_1.default.readFileSync(configurationPath, CONFIG_ENCODING).split(os_1.EOL);
configLines.map(IniFn.trimLine)
    .filter(IniFn.isNotComment)
    .map(IniFn.breakLine)
    .filter(IniFn.isValidLine)
    .forEach(([name, rhs]) => {
    const [workingDir, ...files] = rhs.trim().split(' ').map(IniFn.trimLine);
    const executable = executableManger.add(name.trim(), workingDir, files);
    if (files.length == 0) {
        executable.addAllFilesInWorkingDir();
    }
});
try {
    const executable = executableManger.getByName(executableName);
    executable.run();
}
catch (e) {
    if (e instanceof Error) {
        console.error(e.message);
    }
}
