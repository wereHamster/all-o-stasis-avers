import * as Avers from 'avers'

export class Account
    { login  : string
      role   : string
      email  : string
      name   : string
    }
Avers.definePrimitive(Account, 'login', '')
Avers.definePrimitive(Account, 'role',  'user')
Avers.definePrimitive(Account, 'email', '')
Avers.definePrimitive(Account, 'name',  '')


export class Boulder
    { setter     : string[]
      sector     : string
      grade      : string
      gradeNr    : number
      setDate    : number
      removed    : number
      name       : string
    }

Avers.definePrimitive(Boulder, 'setter',   [])
Avers.definePrimitive(Boulder, 'sector',   'spektrumone')
Avers.definePrimitive(Boulder, 'grade',    'yellow')
Avers.definePrimitive(Boulder, 'gradeNr',  0)
Avers.definePrimitive(Boulder, 'setDate',  0)
Avers.definePrimitive(Boulder, 'removed',  0)
Avers.definePrimitive(Boulder, 'name',     '')

export function
roles(): string[] {
    return ['user', 'setter', 'admin']
}

export function
grades(): string[] {
    return ['yellow', 'green', 'orange', 'blue', 'red', 'white']
}

export function
sectors(): string[] {
    return ['starship', 'bigboss', 'dune', 'klagemauer', 'kurswand',
         'spektrumone', 'spektrumtwo', 'spektrumthree', 'spektrumfour']
}

// FIXME
export function
prettyPrintSector(sectorName: string): string {
    return sectorName
        .replace(/one/i, ' 1')
        .replace(/two/i, ' 2')
        .replace(/three/i, ' 3')
        .replace(/four/i, ' 4')
}


export interface SetterMonthlyStats {
    Yellow?: number
    Green?: number
    Orange?: number
    Blue?: number
    Red?: number
    White?: number
}

const aosNS = Symbol('all-o-stasis')
export const setterMonthlyStats = (aversH: Avers.Handle, setterId: string, year: number, month: number): Avers.Static<SetterMonthlyStats> => {
    const fetch = () => aversH.fetch(`${aversH.apiHost}/stats/${setterId}/${year}/${month}`).then(res => res.json())
    return new Avers.Static<SetterMonthlyStats>(aosNS, `${setterId}-${year}-${month}`, fetch)
}

export interface BoulderStat {
    setOn: Date
    removedOn: void | Date
    setters: string[]
    sector: string
    grade: string
}

export const boulderStats = (aversH: Avers.Handle): Avers.Static<BoulderStat[]> => {
    const fetch = () => aversH.fetch(`${aversH.apiHost}/stats/boulders`)
        .then(res => res.json())
        .then(bss => bss.map(bs => ({
            ...bs,
            setOn: new Date(Date.parse(bs.setOn)),
            removedOn: bs.removedOn ? new Date(Date.parse(bs.removedOn)) : undefined,
        })))

    return new Avers.Static<BoulderStat[]>(aosNS, `boulderStats`, fetch)
}
