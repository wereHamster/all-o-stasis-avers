/*
module Account
( accountView
) where
*/

import * as React from 'react';

import * as Avers from 'avers';
import {App, refresh, navigateTo, navigateToFn} from '../app';

import {Site} from './Components/Site';

import {DropDownInput} from './Components/DropdownInput';

import {Account, roles} from '../storage';

export interface AccountViewProps {
    app: App;
    accountE: Avers.Editable<Account>;
}

class AccountSpec extends React.Component<AccountViewProps, {}> {
    changeAccountName = (e: React.FormEvent<any>) => {
        let value = (e.target as HTMLInputElement).value;
        this.props.accountE.content.name = value;
    }

    changeAccountEmail = (e: React.FormEvent<any>) => {
        let value = (e.target as HTMLInputElement).value;
        this.props.accountE.content.email = value;
    }

     changeAccountLogin = (e: React.FormEvent<any>) => {
        let value = (e.target as HTMLInputElement).value;
        this.props.accountE.content.login = value;
    }

    render() {
        const {app, accountE} = this.props;

        return (
          <div>
            {this.accountHeader(accountE)}
            {this.accountDetailsEditor(accountE)}
          </div>
        );
    }

    accountAdminFields(accountE: Avers.Editable<Account>) : JSX.Element {

        var account = accountE.content;
        if (account.role != 'admin') {
            return;
        }

        return (
          <div className="form-row">
            <div className="label">Role</div>
            <div className="content">
              <DropDownInput object={account} field='role' options={roles()}></DropDownInput>
            </div>
          </div>
        );
    }

    accountHeader(accountE: Avers.Editable<Account>) : JSX.Element {
        if(accountE === undefined ||
           accountE.objectId === undefined) {
            console.log("Undefined account objectId.. Should not happen!");
            return;
        }

        var title = accountE.objectId;
        if(accountE.content.name != '')
            title = accountE.content.name;

        return (
          <div className="boulder-header">
            <div className="login">
              <div className="logo">
                {title}
              </div>
              <p className="about">
                "Customize your account."
              </p>
            </div>
          </div>
        );
    }

    accountDetailsEditor(accountE: Avers.Editable<Account>) : JSX.Element {

        if(accountE === undefined || accountE.objectId === undefined) {
            console.log("Undefined account objectId.. Should not happen!");
            return;
        }

        var account = accountE.content;

        function onClick(e) {
            e.stopPropagation();
        }

        return (
          <div className="account-detail-editor">
            <div className="form">
              <div className="form-row">
                <div className="label">User name</div>
                <div className="content">
                  <input className='wide' type='text' value={account.name}
                         onChange={this.changeAccountName} onClick={onClick}></input>
                </div>
              </div>
              <div className="form-row">
                <div className="label">Email</div>
                <div className="content">
                  <input className='wide' type='text' value={account.email}
                         onChange={this.changeAccountEmail} onClick={onClick}></input>
                </div>
              </div>
               <div className="form-row">
                <div className="label">Login</div>
                <div className="content">
                  <input className='wide' type='text' value={account.login}
                         onChange={this.changeAccountLogin} onClick={onClick}></input>
                </div>
              </div>
              {this.accountAdminFields(accountE)}
            </div>
          </div>
        )
    }
}

var AccountView = React.createFactory(AccountSpec);

export function
accountView(app: App, accountId: string) {
    let accountC = Avers.lookupEditable<Account>(app.data.aversH, accountId);
    let accountE = accountC.get(undefined);

    return (
        <Site app={app}>
            <div className="account">
                {AccountView({ app: app, accountE: accountE })}
            </div>
        </Site>
    );
}
