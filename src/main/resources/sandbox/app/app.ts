import {Component} from 'angular2/core';
import {Http, Headers, HTTP_PROVIDERS} from 'angular2/http'
import 'rxjs/Rx'

@Component({
    selector: 'sandbox-app',
    templateUrl: 'app/app.html' ,
    providers: [HTTP_PROVIDERS] //todo: remove?
})
export class App {
    http: Http
    sources: Array<string>;
    variables: Array<Variable>;

    selectedSourceFilePath: string;
    selectedSourceContent: string;

    private headers = new Headers({'Content-Type': 'application/json'});

    constructor(http: Http) {
        this.http = http;
        http.get('sandbox/sources').map(r => JSON.parse(r._body)).subscribe(sources => {
            this.sources = sources.list;
            this.variables = sources.vars;
        });
    }

    onSourceClick(source: string): void {
        this.http.get(encodeURI('sandbox/sources/' + source)).map(r => JSON.parse(r._body)).subscribe(source => {
            this.selectedSourceFilePath = source.filePath;
            this.selectedSourceContent = source.content;
        });
    }

    onSaveSelectedSourceClick(): void {
        this.http.put(encodeURI('sandbox/sources/' + this.selectedSourceFilePath), JSON.stringify({"content": this.selectedSourceContent}), {headers: this.headers}).subscribe(res => {
            console.log("Updated:");
            console.log(res);
        });
    }

    onSaveVariablesClick(): void {
        let variablesObj: any = {};
        this.variables.forEach(v => variablesObj[v.name] = v.value);

        console.log(variablesObj);
        //todo: implement
    }
}

export class Variable {
    name: string;
    value: string;
    constructor(n: string, v: string) { this.name = n; this.value = v; }
}