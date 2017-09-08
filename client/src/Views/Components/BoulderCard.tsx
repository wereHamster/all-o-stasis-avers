import * as Avers from 'avers';
import * as React from 'react';

import {accountGravatarUrl} from '../Account';
import {App, navigateTo} from '../../app';
import {Account, Boulder, prettyPrintSector} from '../../storage';

export interface BoulderCardProps {
    app: App;
    boulderE: Avers.Editable<Boulder>;
}

export class BoulderCard extends React.Component<BoulderCardProps, {}> {
    onClick = () => {
        navigateTo('/boulder/' + this.props.boulderE.objectId);
    }

    render() {
        const {app, boulderE} = this.props;
        const {objectId, content} = boulderE;

        const setters = content.setter.map(setterId => {
            return Avers.lookupContent<Account>(app.data.aversH, setterId).fmap(account => (
                <img className="boulder-card-setter" src={accountGravatarUrl(account.email)} />
            )).get(undefined);
        }).filter(x => x !== undefined);

        const sectorName = prettyPrintSector(boulderE.content.sector);

        return (
            <div className="boulder-card" onClick={this.onClick}>
                <div className={`boulder-card-id ${content.grade}`}>
                    {boulderE.content.gradeNr}
                </div>
                <div className="boulder-card-meta">
                    <div className="boulder-card-sector">{sectorName}</div>
                    <div className="boulder-card-setters">
                        {setters}
                    </div>
                </div>
            </div>
        );
    }
}
