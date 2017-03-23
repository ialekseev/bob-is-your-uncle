import {Component} from 'angular2/core';
import {Http, HTTP_PROVIDERS} from 'angular2/http'
import 'rxjs/Rx'

@Component({
  selector: 'sandbox-app',
  templateUrl: 'app/app.html',
  providers: [HTTP_PROVIDERS]
})
export class App {
  sources: Array<String>;
  vars: Array<[String, String]>;

  constructor(http: Http) {
     http.get('sandbox/sources').map(r => JSON.parse(r._body)).subscribe(sources => {
        this.sources = sources.list;
        this.vars = sources.vars;
     })
  }
}
