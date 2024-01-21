import { EasyApplication } from '@typinghare/easy-app'
import { ExecutableManager } from './ExecutableManager'
import { Environment } from './Environment'

/**
 * Application.
 */
export class Application extends EasyApplication {
    public constructor(private readonly env: Environment) {
        super([
            ExecutableManager,
        ])
    }

    /**
     * Returns the environment.
     */
    public getEnv(): Environment {
        return this.env
    }
}

