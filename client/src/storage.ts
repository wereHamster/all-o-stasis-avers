/*
module Storage
( Account
, Activity
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

export class Activity
    { actor  : string;
      object : string;
      verb   : string;
      device : string;
    }
Avers.definePrimitive(Activity, 'actor',  '');
Avers.definePrimitive(Activity, 'object', '');
Avers.definePrimitive(Activity, 'verb',   '');
Avers.definePrimitive(Activity, 'device', '');

export class BoulderGrade
    { value : string;
    }

Avers.definePrimitive(BoulderGrade, 'value', '');

export class BoulderComment
    { value : string;
    }

Avers.definePrimitive(BoulderComment, 'value', '');


export class Boulder
    { setter     : string[];
      sector     : string;
      grade      : string;
      gradeNr    : number;
      removed    : Date;
      name       : string;
    }

Avers.definePrimitive(Boulder, 'setter');
Avers.definePrimitive(Boulder, 'sector',   'spektrumone');
Avers.definePrimitive(Boulder, 'grade',    'yellow');
Avers.definePrimitive(Boulder, 'gradeNr',  0);
Avers.definePrimitive(Boulder, 'removed');
Avers.definePrimitive(Boulder, 'name',     '');

