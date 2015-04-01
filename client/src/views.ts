/// <reference path="./ext/react.d.ts" />

/*
module Views
( loadingView
, notFoundView
) where
*/


import {App} from './app';


export function loadingView(app: App) {
    return React.DOM.div
        ( {}
        , 'Loading...'
        );
}

export function notFoundView(app: App) {
    return React.DOM.div
        ( {}
        , 'Not Found '
        , React.DOM.a({ href: '/' }, 'back to index')
        );
}
