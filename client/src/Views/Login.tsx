/*
module Login
( loginView
) where
*/

import * as Avers from 'avers';
import {App, navigateTo, navigateToFn} from '../app';
import {Site} from './Components/Site';

function stopPropagation(e) {
    e.stopPropagation();
}

class LoginView extends React.Component<{ app: App }, { accountId: string }> {

    state = { accountId: '' };

    changeAccountId = (e) => {
        this.setState({ accountId: e.target.value });
    };

    doSignin = (e) => {
        e.stopPropagation();
        Avers.signin(this.props.app.data.session, this.state.accountId, '').then(() => {
            navigateTo('/');
        });
    };

    render() {
        let { app } = this.props;
        let { accountId } = this.state;

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
                  We could not find that account identifier.
                </div>
            ) ;
        }

        return (
            <Site app={app}>
              <div className="login">
                <div className="logo">All-o-stasis</div>
                <div className="account-identifier">
                  <div className="account-identifier-heading">Your account identifier</div>
                  <input className="account-id" type="text" value={accountId} onClick={stopPropagation} onChange={this.changeAccountId} />
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
