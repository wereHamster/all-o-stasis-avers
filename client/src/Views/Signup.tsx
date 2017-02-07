/*
module Signup
( signupView
) where
*/

import * as React from 'react';

import * as Avers from 'avers';
import {App, refresh, navigateToFn} from '../app';
import {Site} from './Components/Site';

var signupInProgress = false;

export function
signupView(app: App) {

    function doSignUp() {
        return e => {
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
        };
    }

    if (app.data.session.objId && signupInProgress === false) {
        return (
            <Site app={app}>
              <div className="login">
                <div className="logo">All-o-stasis</div>
                <div className="about">
                    We generated a unique identifier for your account.
                    Write it down somewhere, you will need it to log in again.
                    If you forget it it you will lose access all-o-stasis.
                    You have been warned.
                </div>
                <div className="about">
                    In order to be able to login with a username/password,
                    see the account prefences. Note that until you set a
                    password, your account is not protected.
                </div>
                <div className="button" onClick={navigateToFn('/')}>
                  Alright, I understand. Now let me continue already.
                </div>
              </div>
            </Site>
        );
    } else {
        var btn, login;
        if (signupInProgress) {
            btn = (
                <div className="signup-in-progress">
                  <div className="text">
                    We are busy opening a new account for you.
                  </div>
                </div>
            );
        } else {
            btn = <div className="button" onClick={doSignUp()}>Sign up</div>;
            login = <div className="button" onClick={navigateToFn('/login')}>I already have an account (login)</div>;
        }

        return (
            <Site app={app}>
              <div className="login">
                <div className="logo">All-o-stasis</div>
                <div className="about">
                    Signing up is as easy as clicking the button below.
                    We don't ask for any personal identifying information.
                </div>
                <div className="about">
                    Once your account has been created you can provide
                    additional information at your discression.
                </div>
                {btn}
                {login}
              </div>
            </Site>
        );
    }
}
