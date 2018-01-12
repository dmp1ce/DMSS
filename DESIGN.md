# Dead Man's Switch System design document

High level overview on how the DMSS (Dead Man's Switch System) works.

# Motiviation

We are in the midst of a migration of secret identifying information into digital and/or online forms. Examples of this would be cryptocurrency wallet keys, password login information and other cryptographic key data. The problem exists of how to pass this information along to trusted parties in the event of emergency or even death. The motivation behind this project is to design and implement an automated system for securely and reliably sharing secrets to trusted individuals as it becomes necessary.

# Overview using an example

DMSS consists of at least three components.

- Owner
- Medium for transporting messages
- Trustees

In the diagram below, Alice is the owner. Bob, Charlie and Dana are the trustees. The message medium is email.

This is only an example. The example could have more or less trustees and the message medium could be something other than email. The only requirement for the messages is that they are end-to-end encrypted.

```
         Three Trustee DMS diagram

 +------+      +---------+     +--------+
 |  Bob |      | Charlie |     |  Dana  |
 +------+      +---------+     +--------+
     ^              ^              ^
     |              |              |
     |              |              |
+---------------------------------------+
|      GPG encrypted email messages     |
+---------------------------------------+
     |              |              |
     |              |              |
     |              v              |
     |          +-------+          |
     +--------->| Alice |<---------+
                +-------+
```

Alice is responsible for setting up the system and creating the documents that will be given to the trustees when one or more of the trigger conditions is met.

Bob, Charlie and Dana are responsible for constructing and decrypting the data when they're notified by the DMSS system.

The email system is how the four parties set up new switches and how the trustees communicated when constructing information required to gain access to the encrypted secrets.

## Setup

To setup the switch, Alice would first determine what the secret information is that she wants to transfer. She also needs to create a document which describes how to use the information.

Alice then needs to choose at least one trustee which will receive the information. In this example she has choosen Bob, Charlie and Dana. Once Alice has the secret documents and the trustees she can now secure the secret and distribute the decryption key to the trustees. This process involves the following steps:

1. Generate symetric passphrase
2. Encrypt secret documents with symetric passphrase
3. Split symetric passphrase between trustees using Shamir's Secret Sharing algorithm
4. Generate several hashes of the symetric passphrase parts using a nonce to verify existance in the future
5. Distribute symetric passphrase parts to trustees
6. Remove all traces of passphrase parts from Alice's computer to prevent access from attackers
7. Generate several hashes of the encrypted documents to verify existance in the future
8. Distribute encrypted secret documents to trustees or hold somewhere for them to be distributed at a later time to make collusion impossible

All files and messages distributed to trustees are end-to-end encrypted with only the receipient having access to decrypt.

## Maintenance checks

After the setup and for the majority of the life of the DMSS, Alice will need to check that everyone has everything they need to decrypt the infromation should the need arise.

Maintenance checks include the following:

- Verify all trustees can communicate with each other securely
- Verify all symetric passphrase parts are still available and not corrupted
- Verify encrypted documents still exist and are not corrupted
- Verify owner still approves of secracy by signing messages which is only possible for owner
- Trigger test switches from time to time to verify system is working and to train trustees on how to use the system

Maintenance is very important to have some confidence that the secret information will be transfered at the time of death.

## Constructing secrets after death

Ultimately, the DMSS will trigger after a configured amount of time, when she fails to prove to her computer or server that she is still alive. At that point, the DMSS will let the trustees know and will distribute the encrypted secrets if not done so already.

At this point, the trustees need to share their symetric passphrase parts in order to create the passphrase which will unlock the encrypted secrets.
