import { DataCollection, Datum } from '@typinghare/extrum'

/**
 * Environment.
 */
export class Environment extends DataCollection<EnvironmentData> {
    public constructor() {
        super({
            workingDir: Datum.of(''),
            outputDir: Datum.of(''),
        })
    }
}

export interface EnvironmentData {
    // Working directory
    workingDir: string,

    // Output directory
    outputDir: string
}