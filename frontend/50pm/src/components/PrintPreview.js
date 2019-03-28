import React from 'react';
import styled from 'styled-components';
import { 
  A4Portrait, 
  PageBreak
} from './Paper';

import EquationList from '../components/EquationList';

const Frame = styled.div`
  margin: 0;
  padding: 0;
`;

const PrintPreview = (props) => {
  let equs = props.equs;
  let lastEqu = equs.pop();

  console.log('PrintPreview: equs = ', equs);
  console.log('PrintPreview: lastEqu = ', lastEqu);
  return (
    <Frame>
      {equs.map(x => (
        <React.Fragment key={x.id}>
          <A4Portrait>
            <EquationList equs={x.equs}/>
          </A4Portrait>
          <PageBreak/>
        </React.Fragment>
      ))}
      {(lastEqu && 
        <A4Portrait>
          <EquationList equs={lastEqu.equs}/>
        </A4Portrait>)}
    </Frame>
  );
}

export default PrintPreview;
