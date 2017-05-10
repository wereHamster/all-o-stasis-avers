/*
module Login
( loginView
) where
*/

import * as React from 'react';

import * as Avers from 'avers';
import {App, navigateTo, navigateToFn} from '../app';
import {Site} from './Components/Site';

function stopPropagation(e) {
    e.stopPropagation();
}

class LoginView extends React.Component<{ app: App }, { accountId: string, secret: string }> {

    state = { accountId: '', secret: '' };

    changeAccountId = (e) => {
        this.setState({ accountId: e.target.value });
    };

    changeSecretId = (e) => {
        this.setState({ secret: e.target.value });
    };

    doSignin = (e) => {
        e.stopPropagation();
        Avers.signin(this.props.app.data.session, this.state.accountId, this.state.secret).then(() => {
            navigateTo('/');
        });
    };

    render() {
        let { app } = this.props;
        let { accountId, secret } = this.state;

        let loginInProgress = app.data.session.transition !== undefined;

        var progressIndicator, failure;

        if (loginInProgress) {
            progressIndicator = (
                <div className="login-progress-indicator">
                  Signing in..
                </div>
            );
        }

        if (app.data.session.lastError && !loginInProgress) {
            failure = (
                <div className="login-failure">
                  We could not find that account identifier/password combination.
                  Please check your credentials and try again.
                </div>
            ) ;
        }

        return (
            <Site app={app}>
              <div className="login">
                <div className="logo">All-o-stasis</div>
                <div className="account-identifier">
                  <div className="account-identifier-heading">Your account identifier and password</div>
                  <input className="account-id" type="text" value={accountId} onClick={stopPropagation} onChange={this.changeAccountId} />
                  <input className="account-id" type="password" value={secret} onClick={stopPropagation} onChange={this.changeSecretId} />
                  {failure}
                  {progressIndicator}
                </div>
                {!loginInProgress && <div className="button" onClick={this.doSignin}>Sign in</div>}
                {!loginInProgress && <div className="button" onClick={navigateToFn('/signup')}>Signup</div>}
              </div>
            </Site>
        );
    }
}

export function
loginView(app: App) {
    return <LoginView app={app} />;
}
