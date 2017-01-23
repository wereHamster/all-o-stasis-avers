/*
module Account
( accountView
) where
*/

/// <reference path="../ext/react.d.ts" />

import * as Avers from 'avers';
import {App, refresh, navigateTo, navigateToFn} from '../app';

import {Site} from './Components/Site';

import {DropDownInput} from './Components/DropdownInput';

import {Account} from '../storage';

enum AccountBody
    { Home
    }

class CreatingEvent {
    constructor
        ( public createEventPromise: Promise<string>
        ) {}
}

interface AccountViewState {
    accountViewBody: AccountBody | CreatingEvent;
}

export interface AccountViewProps {
    app: App;
    accountE : Avers.Editable<Account>;
}

class AccountSpec extends React.Component<AccountViewProps, AccountViewState> {

    initialState(props: AccountViewProps): AccountViewState {
        return { accountViewBody: AccountBody.Home };
    }

    constructor(props) {
        super(props);
        this.state = this.initialState(props);
    }

    render() {
        return (
          <div>
            {this.accountHeader(this.props.accountE)} 
            {this.accountDetailsEditor(this.props.accountE)}
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

    formRow(label: string, content: JSX.Element) : JSX.Element {
        return (
          <div className="form-row">
            <div className="label">
              {label}
            </div>
            <div className="content">
              {content}
            </div>
          </div>
        );
    }

    changeAccountName = (e: __React.FormEvent) => {
        let value = (e.target as HTMLInputElement).value;
        this.props.accountE.content.name = value;
    }

    changeAccountEmail = (e: __React.FormEvent) => {
        let value = (e.target as HTMLInputElement).value;
        this.props.accountE.content.email = value;
    }

     changeAccountLogin = (e: __React.FormEvent) => {
        let value = (e.target as HTMLInputElement).value;
        this.props.accountE.content.login = value;
    }

    accountDetailsEditor(accountE: Avers.Editable<Account>) : JSX.Element {

        if(accountE === undefined || accountE.objectId === undefined) {
            console.log("Undefined account objectId.. Should not happen!");
            return;
        }

        var account = accountE.content;

        // FIXME: role should only be changeable by admins
        var roles = ['user', 'setter', 'admin'];

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

               <div className="form-row">
                <div className="label">Role</div>
                <div className="content">
                  <DropDownInput object={account} field='role' options={roles}></DropDownInput>
                </div>
              </div>
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