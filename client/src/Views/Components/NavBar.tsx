/*
module NavBar
( navBar
) where
*/

import * as React from 'react';

import * as Avers from 'avers';
import {App, navigateTo, navigateToFn} from '../../app';
import {role} from '../../actions';
import * as Boulder from '../Boulder';

import {Account} from '../../storage';

function sideBarItem() {
    return (
      <div className='navbar-item' onClick={navigateToFn('/')}>
        <i className='sidebar icon'></i>
      </div>
    );
}

function homeItem() {
    return (
      <div className='navbar-item' onClick={navigateToFn('/')}>
        <i className='home icon'></i>
      </div>
    );
}

function teamItem() {
    return (
      <div className='navbar-item' onClick={navigateToFn('/team')}>
        <i className='users icon'></i>
      </div>
    );
}

function statsItem() {
    return (
      <div className='navbar-item' onClick={navigateToFn('/stats')}>
        <i className='line chart icon'></i>
      </div>
    );
}

// spacer
function flexItem() {
    return (
      <div className='flex-navbar-item'></div>
    );
}

function signInItem() {
    return (
      <div className='navbar-item' onClick={navigateToFn('/login')}>
        <i className='sign in icon'></i>
        Login
      </div>
    );
}

function doSignOutF(app: App) {
    return e => {
        e.stopPropagation();
        Avers.signout(app.data.session).then(() => {
            navigateTo('/');
        });
    };
}

function accountSettingsDropDownItem(app: App) {
    const accountId = app.data.session.objId;

    let accountC = Avers.lookupContent<Account>(app.data.aversH, accountId);
    let accountE = accountC.get(null);

    let accountDisplayName = "Account";
    if (accountE != null)
        accountDisplayName = accountE.login;

    return (
      <div className='navbar-item'>
          <i className='user icon'></i>
          {accountDisplayName}
        <ul>
          <li>{Boulder.createBoulderItem(app)}</li>
          <li><div onClick={navigateToFn('/account/' + accountId)}>
            <i className='settings icon'></i>Account Settings
          </div></li>
          <li><div onClick={navigateToFn('/account/' + accountId + '/updateSecret')}>
            <i className='key icon'></i>Change Secret
          </div></li>
          <li><div onClick={doSignOutF(app)}>
            <i className='sign out icon'></i>Logout
          </div></li>
        </ul>
      </div>
    );
}

export function
navBar(app: App) {
    if (app.data.session.objId) {
        return (
            <div className='navbar'>
              {homeItem()}
              {teamItem()}
              {flexItem()}
              {accountSettingsDropDownItem(app)}
            </div>
        );
    } else {
        return (
          <div className='navbar'>
            {homeItem()}
            {teamItem()}
            {flexItem()}
            {signInItem()}
          </div>
        );
    }
}

