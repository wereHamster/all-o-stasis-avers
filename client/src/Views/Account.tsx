/*
module Account
( accountView
) where
*/


import * as Avers from 'avers';
import {App, refresh, navigateTo, navigateToFn} from '../app';

import {site} from './Components/Site';

import {DropDownInput} from './Components/DropdownInput';

import {Account} from '../storage';


class AccountView extends React.Component<{ app: App }, { account: Account }> {

    state = { account: '' };

    changeAccountName = (e) => {
        this.setState({ account.name = e.target.value });
    };

    changeAccountEmail = (e) => {
        this.setState({ account.email = e.target.value });
    };

    render() {
        var accountC = Avers.lookupEditable<Account>(
            app.data.aversH, account.accountId);

        var accountE = accountC.get(undefined);

        return (
          <Site app={app}>
            this.accountHeader(accountE)
            this.accountDetailsEditor(accountE)
          </Site>
        );
    };

    accountHeader(accountE: Avers.Editable<Account>) {

        if(accountE === undefined ||
           accountE.objectId === undefined) {
            console.log("Undefined account objectId.. Should not happen!");
            return;
        }

        var title = accountE.objectId;
        if(accountE.content.name != '')
            title = accountE.content.name;

        return (
          <div class="boulder-header">
            <div class="login">
              <div class="logo">
                {title}
              </div>
              <p class="about">
                "Customize your account."
              </p>
            </div>
          </div>
        );
    };

    formRow(label: string, content: React.DOM.Element) : React.DOM.div {
        return (
          <div class="form-row">
            <div class="label">
              {label}
            </div>
            <div class="content">
              {content}
            </div>
          </div>
        );
    };


    accountDetailsEditor(accountE: Avers.Editable<Account>) : React.DOM.div {

        if(accountE === undefined || accountE.objectId === undefined) {
            console.log("Undefined account objectId.. Should not happen!");
            return;
        }

        var account = accountE.content;

        // FIXME: role should only be changeable by admins
        var roles = ['user', 'setter', 'admin'];

        return (
          <div class="account-detail-editor">
            <div class="form">
              <div class="form-row">
                <div class="label">'User name'</div>
                <div class='wide' type='text' value={account.name} onChange={changeAccountName}></div>
              </div>
            </div>
          </div>
        );

                //formRow
                    //( 'User name'
                    //, React.DOM.input
                        //( { className: 'wide', type: 'text', value: account.name,
                            //onChange: function(ev) {
                                //account.name = (<HTMLInputElement>ev.target).value;
                            //}
                        //}
                        //)
                    //)
                //, formRow
                    //( 'Email'
                    //, React.DOM.input
                        //( { className: 'wide', type: 'text', value: account.email,
                            //onChange: function(ev) {
                                //account.email = (<HTMLInputElement>ev.target).value;
                            //}
                        //}
                        //)
                    //)
                //, formRow
                    //( 'Role'
                    //, DropDownInput
                        //( { object: account, field: 'role', options: roles }
                        //)
                    //)
                //)
    };
}

