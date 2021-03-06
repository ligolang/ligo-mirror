import joi from '@hapi/joi';
import { createHash } from 'crypto';
import { Request, Response } from 'express';

import { logger } from '../logger';
import latestSchema from '../schemas/share-latest';
import { storage } from '../storage';

interface ShareBody {
  editor: {
    language: string;
    code: string;
    dirty: boolean;
    title: string;
  };
  compile: {
    entrypoint: string;
    protocol: string;
  };
  dryRun: {
    entrypoint: string;
    parameters: string;
    storage: string;
    protocol: string;
  };
  deploy: {
    entrypoint: string;
    storage: string;
    useTezBridge?: boolean;
    protocol: string;
  };
  evaluateValue: {
    entrypoint: string;
    protocol: string;
  };
  evaluateFunction: {
    entrypoint: string;
    parameters: string;
    protocol: string;
  };
  generateDeployScript: {
    tool: string;
    entrypoint: string;
    storage: string;
    originationAccount: string;
    burnCap: number;
    protocol: string;
  };
}

const validateRequest = (body: any): { value: ShareBody; error?: any } => {
  return joi
    .object({
      editor: joi
        .object({
          language: joi.string().required(),
          code: joi.string().required(),
          dirty: joi.boolean().optional(),
          title: joi.string().allow(''),
        })
        .required(),
      compile: joi.object({
        entrypoint: joi.string().allow(''),
        protocol: joi.string().required(),
      }),
      dryRun: joi.object({
        entrypoint: joi.string().allow(''),
        parameters: joi.any().allow(''),
        storage: joi.any().allow(''),
        protocol: joi.string().required(),
      }),
      deploy: joi.object({
        entrypoint: joi.string().allow(''),
        storage: joi.any().allow(''),
        network: joi.string().allow(''),
        signer: joi.string().allow(''),
        protocol: joi.string().required(),
      }),
      evaluateValue: joi.object({
        entrypoint: joi.string().allow(''),
        protocol: joi.string().required(),
      }),
      evaluateFunction: joi.object({
        entrypoint: joi.string().allow(''),
        parameters: joi.any().allow(''),
        protocol: joi.string().required(),
      }),
      generateDeployScript: joi.object({
        tool: joi.string().allow(''),
        entrypoint: joi.string().allow(''),
        storage: joi.any().allow(''),
        originationAccount: joi.string().allow(''),
        burnCap: joi.number().allow(''),
        protocol: joi.string().required(),
      }),
    })
    .validate(body);
};

function escapeUrl(str: string) {
  return str.replace(/\+/g, '-').replace(/\//g, '_').replace(/=/g, '');
}

export async function shareHandler(req: Request, res: Response) {
  const { error, value } = validateRequest(req.body);

  if (error) {
    res.status(400).json({ error: error.message });
  } else {
    try {
      const versionedShareState = {
        version: latestSchema.VERSION,
        state: value,
      };

      const { error } = latestSchema.validate(versionedShareState);

      if (error) {
        logger.error(
          `${versionedShareState} doesn't match latest schema ${latestSchema.VERSION}`
        );
        res.sendStatus(500);
      } else {
        const fileContent = JSON.stringify(versionedShareState);
        const hash = createHash('md5');
        hash.update(fileContent);
        const digest = escapeUrl(hash.digest('base64'));
        const filename = `${digest}.txt`;

        storage.write(filename, fileContent);

        res.send({ hash: digest });
      }
    } catch (ex) {
      logger.error((ex as Error).message);
      res.sendStatus(500);
    }
  }
}
