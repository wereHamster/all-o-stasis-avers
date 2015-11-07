/// <reference path="./ext/react.d.ts" />

/*
module Views
( loadingView
, notFoundView
) where
*/


import {App} from './app';

import {site} from './Views/Components/Site';
import {navBar} from './Views/Components/NavBar';

export function loadingView(app: App) {
    return React.DOM.div
        ( { className: 'login' }
        , React.DOM.div
            ( { className: 'logo' }
            , 'Loading..'
            )
        );
}

export function notFoundView(app: App) {
    return site
        ( app
        , navBar(app)
        , React.DOM.div
            ( { className: 'login' }
            , React.DOM.div
                ( { className: 'logo' }
                , 'Ooops..'
                )
            , React.DOM.p
                ( { className: 'about' }
                , 'Not Found '
                , React.DOM.a({ href: '/' }, 'back')
                )
            )
        );
}
