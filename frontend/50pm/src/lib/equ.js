import shortid from 'shortid';

function genEqus(n, digitOpt, unknownOpt) {
  let equs = [];

  for (let i = 0; i < n; i++) {
    let equ = genRandomEqu(digitOpt, unknownOpt);
    equs.push(equ);
  }

  console.log('genEqus.length = ', equs.length);
  return {id: shortid.generate(), equs: equs};
}

function genEqusList(m, n, digitOpt, unknownOpt) {
  let equsList = [];
  for (let i = 0; i < m; i++) {
    equsList.push(genEqus(n, digitOpt, unknownOpt));
  }

  console.log('genEqusList.length = ', equsList.length);
  return equsList;
}

export { genEqus, genEqusList };

// Utilities

function genRandomEqu(digitOpt, unknownOpt) {
  let n1x = 0, n2x = 0;
  if (digitOpt[0] === true && digitOpt[1] === false && digitOpt[2] === false) {
    n1x = genRandomInt(0, 10);
    n2x = genRandomInt(0, 10);
  } else if (digitOpt[0] === false && digitOpt[1] === true && digitOpt[2] === false) {
    n1x = genRandomInt(10, 100);
    n2x = genRandomInt(10, 100);
  } else {
    n1x = genRandomInt(100, 1000);
    n2x = genRandomInt(100, 1000);
  }

  //console.log('digitOpt = ', digitOpt);
  //console.log('genRandomEqu: [n1x, n2x] = ', n1x, n2x);

  let op = genRandomInt(0, 2);
  let numbers = adjustN1N2(n1x, n2x, op);
  let [n1, n2, n3] = adjustUnknown(numbers, unknownOpt);

  return {id: shortid.generate(), equ: [n1, op, n2, n3]};
}

function genRandomInt(min, max) {
  return Math.floor(min) + Math.floor( Math.random() * Math.floor(max - min));
}

function adjustN1N2(n1, n2, op) {
  if (op === 0) 
    return [n1, n2, n1 + n2];
  else if (op === 1)
    if (n1 - n2 < 0)
      return [n2, n1, n2 - n1];
    else 
      return [n1, n2, n1 - n2];
  else
  {
    console.log("ERROR: operation beyond +/- not supported yet!");
    return [null, null, null];
  }   
}

function adjustUnknown([n1, n2, n3], unknownOpt) {
  if (unknownOpt[0] === true && unknownOpt[1] === false && unknownOpt[2] === false) {
    let dice = Math.random()
    if (dice < .5)
      return [null, n2, n3];
    else 
      return [n1, null, n3];
  }
  else if (unknownOpt[0] === false && unknownOpt[1] === true && unknownOpt[2] === false)
    return [n1, n2, null];
  else {
    let dice = Math.random()
    if (dice < .333)
      return [null, n2, n3];
    else if (dice < .666)
      return [n1, null, n3];
    else
      return [n1, n2, null];
  }
}


