import { NgModule }      from '@angular/core';
import { BrowserModule } from '@angular/platform-browser';
import { FormsModule }   from '@angular/forms';
import { HttpModule } from '@angular/http';
import { NgbModule } from '@ng-bootstrap/ng-bootstrap';
import { AppComponent } from './app.component';
import {ContextMenuModule } from 'angular2-contextmenu';
import { AceEditorModule } from 'ng2-ace-editor';
import {InlineEditorModule} from 'ng2-inline-editor';

@NgModule({
    imports: [
        BrowserModule, //todo: do I need all of them?
        FormsModule,
        HttpModule,
        NgbModule.forRoot(),
        ContextMenuModule.forRoot({
            useBootstrap4: true
        }),
        AceEditorModule,
        InlineEditorModule
    ],
    declarations: [ AppComponent ],
    bootstrap:    [ AppComponent ]
})
export class AppModule {
}
