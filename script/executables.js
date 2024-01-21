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
exports.executables = void 0;
const ocaml_1 = require("./ocaml");
const path_1 = __importDefault(require("path"));
const fs = __importStar(require("fs"));
const environment = new ocaml_1.Environment(
// <root>
path_1.default.join(__dirname, '..'), 
// <root>/script/target
path_1.default.join(__dirname, 'target'));
const executableManger = new ocaml_1.ExecutableManager(environment);
const ocamlMakeFilepath = path_1.default.join(environment.workingDir, 'OCamlMake.ini');
const lines = fs.readFileSync(ocamlMakeFilepath).toString('utf-8').split('\n');
lines.forEach((line) => {
    line = line.trim();
    // Comment line
    if (line.startsWith('#'))
        return;
    const sp = line.split('=');
    if (sp.length != 2)
        return;
    const [name, rhs] = sp;
    const [workingDir, ...files] = rhs.trim().split(' ');
    executableManger.add(name.trim(), workingDir, files.map((file) => file.trim()));
});
exports.executables = executableManger.getExecutables();
