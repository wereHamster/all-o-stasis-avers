/*
module Storage
( Account
, Boulder
) where
*/


import * as Avers from 'avers';


export class Account
    { login  : string;
      role   : string;
      email  : string;
      name   : string;
    }
Avers.definePrimitive(Account, 'login', '');
Avers.definePrimitive(Account, 'role',  'user');
Avers.definePrimitive(Account, 'email', '');
Avers.definePrimitive(Account, 'name',  '');


export class Boulder
    { setter     : string[];
      sector     : string;
      grade      : string;
      gradeNr    : number;
      setDate    : number;
      removed    : number;
      name       : string;
    }

Avers.definePrimitive(Boulder, 'setter',   []);
Avers.definePrimitive(Boulder, 'sector',   'spektrumone');
Avers.definePrimitive(Boulder, 'grade',    'yellow');
Avers.definePrimitive(Boulder, 'gradeNr',  0);
Avers.definePrimitive(Boulder, 'setDate',  0);
Avers.definePrimitive(Boulder, 'removed',  -1);
Avers.definePrimitive(Boulder, 'name',     '');

export function
roles() : string[] {
    return ['user', 'setter', 'admin'];
}

export function
grades() : string[] {
    return ['yellow', 'green', 'orange', 'blue', 'red', 'white'];
}

export function
sectors() : string[] {
    return ['starship', 'bigboss', 'dune', 'klagemauer', 'kurswand',
         'spektrumone', 'spektrumtwo', 'spektrumthree', 'spektrumfour'];
}

//FIXME
export function
prettyPrintSector(sectorName : string) : string {
    return sectorName
        .replace(/one/i, ' 1')
        .replace(/two/i, ' 2')
        .replace(/three/i, ' 3')
        .replace(/four/i, ' 4');
}

