import { TezosToolkit } from '@taquito/taquito';
import { TezBridgeWallet } from '@taquito/tezbridge-wallet';
import { Dispatch } from 'redux';

import { compileContract, compileStorage, deploy, getErrorMessage } from '../../services/api';
import { AppState } from '../app';
import { MichelsonFormat } from '../compile';
import { DoneLoadingAction, UpdateLoadingAction } from '../loading';
import { ChangeContractAction, ChangeOutputAction } from '../result';
import { CommandType } from '../types';
import { CancellableAction } from './cancellable';

const Tezos = new TezosToolkit('https://api.tez.ie/rpc/delphinet');
Tezos.setProvider({
  wallet: new TezBridgeWallet()
});

export class DeployAction extends CancellableAction {
  async deployWithTezBridge(dispatch: Dispatch, getState: () => AppState) {
    dispatch({ ...new UpdateLoadingAction('Compiling contract...') });

    const { Editor: editorState, Deploy: deployState } = getState();

    const michelsonCode = await compileContract(
      editorState.language,
      editorState.code,
      deployState.entrypoint,
      MichelsonFormat.Json
    );

    if (this.isCancelled()) {
      return;
    }

    dispatch({ ...new UpdateLoadingAction('Compiling storage...') });
    const michelsonStorage = await compileStorage(
      editorState.language,
      editorState.code,
      deployState.entrypoint,
      deployState.storage,
      MichelsonFormat.Json
    );

    if (this.isCancelled()) {
      return;
    }

    dispatch({ ...new UpdateLoadingAction('Waiting for TezBridge wallet...') });

    const op = await Tezos.wallet.originate({
      code: JSON.parse(michelsonCode.result),
      init: JSON.parse(michelsonStorage.result)
    }).send();

    if (this.isCancelled()) {
      return;
    }

    dispatch({
      ...new UpdateLoadingAction('Deploying to delphinet network...')
    });
    return {
      address: (await op.contract()).address,
      storage: michelsonStorage.result
    };
  }

  async deployOnServerSide(dispatch: Dispatch, getState: () => AppState) {
    dispatch({
      ...new UpdateLoadingAction('Deploying to delphinet network...')
    });

    const { Editor: editorState, Deploy: deployState } = getState();

    return await deploy(
      editorState.language,
      editorState.code,
      deployState.entrypoint,
      deployState.storage
    );
  }

  getAction() {
    return async (dispatch: Dispatch, getState: () => AppState) => {
      const { Deploy } = getState();

      try {
        const contract = Deploy.useTezBridge
          ? await this.deployWithTezBridge(dispatch, getState)
          : await this.deployOnServerSide(dispatch, getState);

        if (!contract || this.isCancelled()) {
          return;
        }

        dispatch({
          ...new ChangeContractAction(contract.address, CommandType.Deploy)
        });
        dispatch({
          ...new ChangeOutputAction(contract.storage, CommandType.Deploy, false)
        });
      } catch (ex) {
        if (this.isCancelled()) {
          return;
        }
        dispatch({
          ...new ChangeContractAction('', CommandType.Deploy)
        });
        dispatch({
          ...new ChangeOutputAction(
            `Error: ${getErrorMessage(ex)}`,
            CommandType.Deploy,
            true
          )
        });
      }

      dispatch({ ...new DoneLoadingAction() });
    };
  }
}
