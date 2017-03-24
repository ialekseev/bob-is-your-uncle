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
    variables: Array<NameValuePair<String, String>>;

    selectedSourceFilePath: string;
    selectedSourceContent: string;

    private headers = new Headers({'Content-Type': 'application/json'});

    constructor(http: Http) {
        this.http = http;
        http.get('sandbox/sources').map(r => JSON.parse(r._body)).subscribe(sources => {
            this.sources = sources.list;
            this.variables = sources.vars; //todo: change server-side API so that server-side tuple is serialized into an object having 'name', 'value' properties
        })
    }

    onSourceClick(source: string): void {
        this.http.get(encodeURI('sandbox/sources/' + source)).map(r => JSON.parse(r._body)).subscribe(source => {
            this.selectedSourceFilePath = source.filePath;
            this.selectedSourceContent = source.content;
        })
    }

    onSaveSelectedSourceClick(): void {
        this.http.put(encodeURI('sandbox/sources/' + this.selectedSourceFilePath), JSON.stringify({"content": this.selectedSourceContent}), {headers: this.headers}).subscribe(res => {
            console.log("Updated:")
            console.log(res)
        })
    }
}

export class NameValuePair<T1, T2> {
    name: T1;
    value: T2;
    constructor(n: T1, v: T2) { this.name = n; this.value = v; }
}