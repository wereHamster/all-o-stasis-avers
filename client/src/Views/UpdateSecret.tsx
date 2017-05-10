/*
module Login
( UpdateSecretView
) where
*/

import * as React from 'react';

import * as Avers from 'avers';
import {App, navigateTo, navigateToFn} from '../app';
import {Site} from './Components/Site';

import {Account} from '../storage';

function stopPropagation(e) {
    e.stopPropagation();
}

class UpdateSecretView extends React.Component<{ app: App, accountE: Avers.Editable<Account> }, { newSecret: string, newSecretConfirm: string }> {

    state = { accountId: '', newSecret: '', newSecretConfirm: '' };

    changeNewSecret = (e) => {
        this.setState({ newSecret: e.target.value });
    };

    changeNewSecretConfirm = (e) => {
        this.setState({ newSecretConfirm: e.target.value });
    };

    doUpdateSecret = (e) => {
        e.stopPropagation();
        if (this.state.newSecret == this.state.newSecretConfirm) {
            Avers.changeSecret(this.props.app.data.session,
                               this.state.newSecret).then(() => {
                navigateTo('/');
            });
        } else {
            // FIXME set error
        }
    };

    render() {
        let { app } = this.props;
        let { accountId  } = this.state;

        let updateInProgress = app.data.session.transition !== undefined;

        var progressIndicator, failure;

        if (updateInProgress) {
            progressIndicator = (
                <div className="login-progress-indicator">
                  Updating Secret..
                </div>
            );
        }

        if (app.data.session.lastError && !updateInProgress) {
            failure = (
                <div className="login-failure">
                  The password is not updated.. Please try again.
                </div>
            ) ;
        }

        return (
            <Site app={app}>
              <div className="login">
                <div className="logo">Update Secret</div>
                <div className="account-identifier">
                  <input className="account-id" type="password" onClick={stopPropagation} onChange={this.changeNewSecret} />
                  <input className="account-id" type="password" onClick={stopPropagation} onChange={this.changeNewSecretConfirm} />
                  {failure}
                  {progressIndicator}
                </div>
                {!updateInProgress && <div className="button" onClick={this.doUpdateSecret}>Update Secret</div>}
              </div>
            </Site>
        );
    }
}

export function
updateSecretView(app: App, accountId: string) {
    let accountC = Avers.lookupEditable<Account>(app.data.aversH, accountId);
    let accountE = accountC.get(undefined);

    return <UpdateSecretView app={app} accountE={accountE} />;
}
