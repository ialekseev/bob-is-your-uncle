System.config({
    //use typescript for compilation
    transpiler: 'typescript',
    //typescript compiler options
    typescriptOptions: {
        emitDecoratorMetadata: true
    },
    paths: {
        'npm:': 'https://unpkg.com/'
    },
    //map tells the System loader where to look for things
    map: {

        'app': './app',

        '@angular/core': 'npm:@angular/core/bundles/core.umd.js',
        '@angular/common': 'npm:@angular/common/bundles/common.umd.js',
        '@angular/compiler': 'npm:@angular/compiler/bundles/compiler.umd.js',
        '@angular/platform-browser': 'npm:@angular/platform-browser/bundles/platform-browser.umd.js',
        '@angular/platform-browser-dynamic': 'npm:@angular/platform-browser-dynamic/bundles/platform-browser-dynamic.umd.js',
        '@angular/http': 'npm:@angular/http/bundles/http.umd.js',
        '@angular/router': 'npm:@angular/router/bundles/router.umd.js',
        '@angular/forms': 'npm:@angular/forms/bundles/forms.umd.js',
        '@angular/animations': 'npm:@angular/animations/bundles/animations.umd.js',
        '@angular/platform-browser/animations': 'npm:@angular/platform-browser/bundles/platform-browser-animations.umd.js',
        '@angular/animations/browser': 'npm:@angular/animations/bundles/animations-browser.umd.js',

        '@angular/core/testing': 'npm:@angular/core/bundles/core-testing.umd.js',
        '@angular/common/testing': 'npm:@angular/common/bundles/common-testing.umd.js',
        '@angular/compiler/testing': 'npm:@angular/compiler/bundles/compiler-testing.umd.js',
        '@angular/platform-browser/testing': 'npm:@angular/platform-browser/bundles/platform-browser-testing.umd.js',
        '@angular/platform-browser-dynamic/testing': 'npm:@angular/platform-browser-dynamic/bundles/platform-browser-dynamic-testing.umd.js',
        '@angular/http/testing': 'npm:@angular/http/bundles/http-testing.umd.js',
        '@angular/router/testing': 'npm:@angular/router/bundles/router-testing.umd.js',

        '@ng-bootstrap/ng-bootstrap': 'npm:@ng-bootstrap/ng-bootstrap@latest/bundles/ng-bootstrap.js',

        'angular2-contextmenu': 'npm:angular2-contextmenu@latest/angular2-contextmenu.js',

        'ng2-ace-editor': 'npm:ng2-ace-editor@latest/ng2-ace-editor.js',
        'brace': 'npm:brace',
        'w3c-blob': 'npm:w3c-blob',

        'ng2-inline-editor': 'npm:ng2-inline-editor@latest/dist/index.js',

        'rxjs': 'npm:rxjs',
        'typescript': 'npm:typescript@2.2.1/lib/typescript.js'
    },
    //packages defines our app package
    packages: {
        app: {
            main: './main.ts',
            defaultExtension: 'ts'
        },
        rxjs: {
            defaultExtension: 'js'
        }
    }
});
