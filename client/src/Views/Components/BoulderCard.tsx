import * as React from 'react';
import * as Avers from 'avers';

import {App, refresh, navigateTo, navigateToFn} from '../../app';
import {Account, Boulder, prettyPrintSector} from '../../storage';

export function tileHeader(app: App, boulderId: string) : JSX.Element {

    var colorClass = "unknown";
    var gradeNumber = 0;

    // FIXME: group by date
    let meta : JSX.Element = Avers.lookupEditable<Boulder>(
      app.data.aversH, boulderId).fmap(boulderE => {
        var boulder = boulderE.content;

        colorClass = boulder.grade.toLowerCase();
        gradeNumber = boulder.gradeNr;

        var setters = "";
        for (var i = 0; i < boulder.setter.length; i++) {
            setters += Avers.lookupContent<Account>(app.data.aversH, boulder.setter[i]).fmap(account => {

                var name = boulder.setter[i].substring(0, 7);
                if (account.name != "")
                    name = account.name;

                return name + ', ';
            }).get(undefined);
        }

        setters = setters.substring(0, setters.length - 2);

        let boulderDate = boulderE.createdAt.getDate() + '.' +
            (boulderE.createdAt.getMonth() + 1) + '.' +
            boulderE.createdAt.getFullYear();

        return (
            <div className="boulder-tile-header-meta">
              {boulderDate + " :: " + boulder.sector + " :: " + setters}
            </div>
        );
    }).get(undefined);

    var boulderClass = 'boulder-tile-header ' + colorClass;
    var boulderLink  = navigateToFn('/boulder/' + boulderId);

    return (
        <div className={boulderClass} onClick={boulderLink}>
          <div className="boulder-tile-header-indicator">{gradeNumber}</div>
          {meta}
        </div>
    );
}

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
                <img className="boulder-card-setter" src="http://ba.iff.im/avatars/W.jpg" />
            )).get(undefined);
        }).filter(x => x !== undefined);

        const sectorName = prettyPrintSector(boulderE.content.sector);

        return (
            <div className="boulder-card" onClick={this.onClick}>
                <div className={`boulder-card-id ${content.grade}`}>
                    {boulderE.content.gradeNr}
                </div>
                <div>
                    <div className="boulder-card-sector">{sectorName}</div>
                    <div className="boulder-card-setters">
                        {setters}
                    </div>
                </div>
            </div>
        );
    }
}
