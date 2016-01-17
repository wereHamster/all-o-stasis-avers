/*
module Account
( accountView
) where
*/


import * as Avers from 'avers';
import {App, refresh, navigateTo, navigateToFn} from '../app';

import {site} from './Components/Site';
import {navBar} from './Components/NavBar';

import {DropDownInput} from './Components/DropdownInput';

import {Account} from '../storage';



export function
accountView(app: App, accountId: string) {

    var accountC = Avers.lookupEditable<Account>(
        app.data.aversH, accountId);

    var accountE = accountC.get(undefined);

    return site
        ( app
        , navBar(app)
        , accountHeader(accountE)
        , accountDetailsEditor(accountE)
        );
}


function accountHeader(accountE: Avers.Editable<Account>) {

    if(accountE.objectId === undefined)
        return;

    var title = accountE.objectId;
    if(accountE.content.user != '')
        title = accountE.content.user;


    return React.DOM.div
          ( { className: 'boulder-header' }
            , React.DOM.div
            ( { className: 'login' }
            , React.DOM.div
                ( { className: 'logo' }
                , title
                )
            , React.DOM.p
                ( { className: 'about' }
                , 'Customize your account.'
                )
            )
          );
}

function formRow(label, content) {
     return React.DOM.div
         ( { className: 'form-row' }
         , React.DOM.div
             ( { className: 'label' }
             , label
             )
         , React.DOM.div
             ( { className: 'content' }
             , content
             )
         );
}

function accountDetailsEditor(accountE: Avers.Editable<Account>) {
     var account = accountE.content;

     // FIXME: role should only be changeable by admins
     var roles = ['user', 'setter', 'admin'];

     return React.DOM.div
        ( { className: 'boulder-details-editor' }
        , React.DOM.div
             ( { className: 'form' }
             , formRow
                 ( 'User name'
                 , React.DOM.input
                     ( { className: 'wide', type: 'text', value: account.user, onChange: function(ev) {
                         account.user = (<HTMLInputElement>ev.target).value;
                         }
                     }
                     )
                 )
             , formRow
                 ( 'Email'
                 , React.DOM.input
                     ( { className: 'wide', type: 'text', value: account.email, onChange: function(ev) {
                         account.email = (<HTMLInputElement>ev.target).value;
                         }
                     }
                     )
                 )
             , formRow
                 ( 'Role'
                 , DropDownInput({ object: account, field: 'role', options: roles})
                 )
             )
        );
}
