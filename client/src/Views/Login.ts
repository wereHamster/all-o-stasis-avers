/*
module Login
( loginView
) where
*/


import * as Avers from '../lib/avers';
import {App, refresh, navigateTo, navigateToFn} from '../app';
import * as Site from './Components/Site';
import * as NavBar from './Components/NavBar';


//FIXME: check with battery tracker (class)

var accountId = '';

function stopPropagation(e) {
    e.stopPropagation();
}

export function
loginView(app: App) {

    function changeAccountId(e) {
        accountId = e.target.value;
        refresh(app);
    }

    function doSignin(e) {
        e.stopPropagation();
        Avers.signin(app.data.session, accountId, '').then(() => {
            navigateTo('/');
        });
    }

    accountId = app.data.session.objId;

    return Site.site
        ( app
        , NavBar.navBar(app)
        , React.DOM.div
            ( { className: 'login' }
            , React.DOM.div
                ( { className: 'logo' }
                , 'All-o-stasis'
                )
            , React.DOM.p
                ( { className: 'about' }
                , 'Use your account identifier or user name to login.'
                )
            , React.DOM.input
                ( { className: 'wide', type: 'text', value: accountId, onClick: stopPropagation, onChange: changeAccountId }
                )
            , React.DOM.div
                ( { className: 'button', onClick: doSignin }
                , 'Sign in'
                )
            , React.DOM.div
                ( { className: 'button', onClick: navigateToFn('/signup') }
                , 'Sign up'
                )
            )
        );
}
