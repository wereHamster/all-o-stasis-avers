/*
module Signup
( signupView
) where
*/


import * as Avers from '../lib/avers';
import {App, refresh, navigateToFn} from '../app';
import {site} from './Components/Site';
import * as NavBar from './Components/NavBar';

var signupInProgress = false;

export function
signupView(app: App) {

    function doSignUp(e) {
        e.stopPropagation();

        signupInProgress = true;
        Avers.startNextGeneration(app.data.aversH);

        var signupStartedAt = Date.now();

        Avers.signup(app.data.session, '').then(accountId => {
            return Avers.signin(app.data.session, accountId, '');
        }).then(() => {
            var signupDuration = Date.now() - signupStartedAt;
            var delay = Math.max(0, 3000 - signupDuration);
            setTimeout(() => {
                signupInProgress = false;
                Avers.startNextGeneration(app.data.aversH);
            }, delay);
        }).catch(err => {
            alert('Sign up failed: ' + err);
        });
    }

    if (app.data.session.objId && signupInProgress === false) {
        return site
            ( app
            , NavBar.navBar(app)
            , React.DOM.div
                ( { className: 'login' }
                , React.DOM.div
                    ( { className: 'logo' }
                    , 'All-o-statsis'
                    )
                , React.DOM.p
                    ( { className: 'about' }
                    , "We generated a unique identifier for your account. "
                    , "Write it down somewhere, you will need it to log in again. "
                    , "If you forget it it you will lose access all-o-stasis. "
                    , "You have been warned."
                    )
                , React.DOM.p
                    ( { className: 'about' }
                    , "In order to be able to login with a username/password, "
                    , "see the account prefences. Note that until you set a "
                    , "password, your account is not protected."
                    )
                , React.DOM.div
                    ( { className: 'account-identifier' }
                    , React.DOM.div({ className: 'account-identifier-heading' }, 'Your account identifier')
                    , React.DOM.div({ className: 'account-identifier-value ' }, app.data.session.objId)
                    )
                , React.DOM.div
                    ( { className: 'button', onClick: () => { navigateToFn('/'); } }
                    , 'Alright, I understand. Now let me continue already.'
                    )
                )
            );
    } else {
        var btn, other;
        if (signupInProgress) {
            btn = React.DOM.div
                ( { className: 'signup-in-progress' }
                , React.DOM.div
                    ( { className: "indicator" }
                    , React.DOM.div
                        ( { className: "sk-spinner sk-spinner-double-bounce" }
                        , React.DOM.div({ className: "sk-double-bounce1" })
                        , React.DOM.div({ className: "sk-double-bounce2" })
                        )
                    )
                , React.DOM.div
                    ( { className: 'text' }
                    , 'We are busy opening a new account for you.'
                    )
                );
        } else {
            btn = React.DOM.div
                ( { className: 'button', onClick: doSignUp }
                , 'Sign up'
                );
            other = React.DOM.div
                ( { className: 'button', onClick: () => { navigateToFn('/login'); } }
                , "I already have an account (Login)"
                );
        }

        return site
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
                    , "Signing up is as easy as clicking the button below. "
                    , "We don't ask for any personal identifying information. "
                    )
                , React.DOM.p
                    ( { className: 'about' }
                    , "Once your account has been created you can provide "
                    , "additional information at your discression."
                    )
                , btn
                , other
                )
            );
    }
}
