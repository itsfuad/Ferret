// @ts-check
import { defineConfig } from 'astro/config';
import starlight from '@astrojs/starlight';
import ferretGrammarJson from './syntax/fer.tmLanguage.json';
import d2 from 'astro-d2';

// Cast to any to avoid type errors with the complex grammar structure
const ferretGrammar = /** @type {any} */ (ferretGrammarJson);

// https://astro.build/config
export default defineConfig({
    output: 'static',
    trailingSlash: 'ignore',
    integrations: [starlight({
        title: 'Ferret',
        description: 'A modern, type-safe programming language',
        favicon: '/favicon.png',  // Change to '/favicon.png' or '/favicon.ico' if using different format
        disable404Route: true, // Use custom 404 page instead of Starlight's
        lastUpdated: true,
        editLink: {
            baseUrl: 'https://github.com/itsfuad/Ferret/edit/main/website/',
        },
        social: [
            { icon: 'github', label: 'GitHub', href: 'https://github.com/itsfuad/Ferret' }
        ],
        customCss: [
            './src/styles/custom.css',
        ],
        defaultLocale: 'root',
        locales: {
            root: {
                label: 'English',
                lang: 'en',
            },
        },
        // Use a single theme for both code snippets and playground for visual consistency.
        expressiveCode: {
            themes: ['one-dark-pro', 'one-light'], // Change this to your preferred built-in Shiki theme
            shiki: {
                langs: [ferretGrammar],
            },
        },
        components: {
            // Override the Head component to add View Transitions
            Head: './src/components/Head.astro',
            // Modern shadcn-style theme toggle
            ThemeSelect: './src/components/ThemeSelect.astro',
            // Use ferret image instead of text
            SiteTitle: './src/components/SiteTitle.astro',
            // Use custom navbar for all pages
            Header: './src/components/Header.astro',
            // Custom mobile TOC with integrated burger menu
            MobileTableOfContents: './src/components/MobileTableOfContents.astro',
        },

        sidebar: [
            {
                label: 'Getting Started',
                items: [
                    { label: 'Introduction', slug: 'getting-started/introduction' },
                    { label: 'Installation', slug: 'getting-started/installation' },
                    { label: 'Hello World', slug: 'getting-started/hello-world' },
                ],
            },
            {
                label: 'Basics',
                items: [
                    { label: 'Variables & Constants', slug: 'basics/variables' },
                    { label: 'Data Types', slug: 'basics/types' },
                    { label: 'Operators', slug: 'basics/operators' },
                    { label: 'Comments', slug: 'basics/comments' },
                ],
            },
            {
                label: 'Control Flow',
                items: [
                    { label: 'If Statements', slug: 'control-flow/if-statements' },
                    { label: 'Loops', slug: 'control-flow/loops' },
                    { label: 'Match Expressions', slug: 'control-flow/match' },
                ],
            },
            {
                label: 'Functions',
                items: [
                    { label: 'Function Basics', slug: 'functions/basics' },
                    { label: 'Parameters & Returns', slug: 'functions/parameters' },
                ],
            },
            {
                label: 'Type System',
                items: [
                    { label: 'Structs', slug: 'type-system/structs' },
                    { label: 'Enums', slug: 'type-system/enums' },
                    { label: 'Interfaces', slug: 'type-system/interfaces' },
                    { label: 'Optional Types', slug: 'type-system/optionals' },
                    { label: 'Maps', slug: 'type-system/maps' },
                    { label: 'Methods', slug: 'type-system/methods' },
                    { label: 'References', slug: 'type-system/references' },
                ],
            },
            {
                label: 'Advanced',
                items: [
                    { label: 'Error Handling', slug: 'advanced/errors' },
                    { label: 'Generics', slug: 'advanced/generics' },
                    { label: 'Modules', slug: 'advanced/modules' },
                    { label: 'Practical Examples', slug: 'advanced/practical'}
                ],
            },
        ],
    }), d2({
      sketch: true,
      layout: 'elk',
      // Disable generating diagrams when deploying on Vercel.
      skipGeneration: !!process.env['VERCEL'],
    })],

    vite: {
        plugins: [],
    },
});