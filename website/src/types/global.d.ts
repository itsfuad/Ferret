import * as monaco from 'monaco-editor';

declare global {
    interface Window {
        ferretCompile?: (
            code: string,
            debug: boolean,
        ) => { success: boolean; output?: string; error?: string };
        ferretWasmVersion?: string;
        ferretEditor?: monaco.editor.IStandaloneCodeEditor;
        MonacoEnvironment?: {
            getWorker: () => Worker;
        };
    }
    
    class Go {
        importObject: any;
        run(instance: WebAssembly.Instance): Promise<void>;
    }

    // Svelte 5 Runes
    function $state<T>(initial: T): T;
    function $state<T>(initial: () => T): T;
    namespace $state {
        function raw<T>(value: T): T;
    }

    function $derived<T>(fn: () => T): T;
    function $derived<T>(fn: () => T, deps: any[]): T;
    namespace $derived {
        function by<T>(fn: () => T): T;
    }

    function $effect(fn: () => void | (() => void)): void;
    function $effect.pre(fn: () => void | (() => void)): void;
    function $effect.root(fn: () => void | (() => void)): void;

    function $props<T>(): T;
    function $bindable<T>(initial?: T): T;
}

export {};

