import * as React from 'react';

import * as Avers from 'avers';
import {Data, App, config, infoTable, refresh, loadView} from './app';

import {loadingView, notFoundView} from './views';
import {accountView} from './Views/Account';
import {boulderView} from './Views/Boulder';
import {homeView}    from './Views/Home';
import {loginView}   from './Views/Login';
import {sectorView}  from './Views/Sector';
import {teamView}    from './Views/Team';
import {signupView}  from './Views/Signup';
import {catalogView} from './Views/Catalog';
import {updateSecretView}   from './Views/UpdateSecret';


function mkApp(): App {
    let aversH = new Avers.Handle
        ( config.apiHost
        , (<any>window).fetch.bind(window)
        , path => new WebSocket('ws:' + config.apiHost + path)
        , window.performance.now.bind(window.performance)
        , infoTable
        );

    let data = new Data(aversH);

    return new App
        ( document.getElementById('root')
        , data
        , loadingView
        );
}

export default function() {
    console.info('Starting app...');

    // Create the application instance. Pass all required configuration to the
    // constructor.
    let app = mkApp();

    // Attach a listener to watch for changes. Refresh the application UI
    // when any change happens. This is the main rendering loop of the
    // application. The 'console.info' shows you how frequently the application
    // data changes.
    Avers.attachGenerationListener(app.data.aversH, () => {
        // console.info('Generation', app.data.aversH.generationNumber);
        refresh(app);
    });

    // This template uses page.js for the router. If you target modern browsers
    // you may get away with a more straightforward implementation and listen
    // to the hashchange events directly.
    setupRoutes(app);

    // The app template makes use of the Avers Session. First thing we try to
    // do is to restore an existing session. The views will use the information
    // that is stored in the session object to determine what to show.
    Avers.restoreSession(app.data.session);

    // Expose some useful tools on 'window' to make them easily accessible from
    // the developer console. This part is entirely optional, though
    // I recommend to keep it even in production. It doesn't add any significant
    // overhead and gives you easy access to the application state.
    (<any>window).Avers = Avers;
    (<any>window).app   = app;
}


declare var page;
function setupRoutes(app: App) {
    page('/', () => {
        loadView(app, app => {
            return homeView(app);
        });
    });

    page('/signup', function() {
        loadView(app, app => {
            return signupView(app);
        });
    });

    page('/login', function() {
        loadView(app, () => {
            return loginView(app);
        });
    });

    page('/sector', function(ctx) {
        loadView(app, app => {
            return sectorView(app);
         });
    });

    page('/boulder/:boulderId', function(ctx) {
        loadView(app, app => {
            return boulderView(app, ctx.params.boulderId);
         });
    });

    page('/account/:accountId', function(ctx) {
        loadView(app, app => {
            return accountView(app, ctx.params.accountId);
         });
    });

    page('/account/:accountId/updateSecret', function(ctx) {
        loadView(app, app => {
            return updateSecretView(app, ctx.params.accountId);
         });
    });

    page('/team', function() {
        loadView(app, app => {
            return teamView(app);
        });
    });

    page('/_catalog', function() {
        loadView(app, app => {
            return catalogView();
        });
    });

    page('*', () => {
        loadView(app, app => {
            return notFoundView(app);
        });
    });

    page();
}
