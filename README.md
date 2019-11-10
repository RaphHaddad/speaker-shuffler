# README

This is a speaker shuffling application designed for speaking courses. Specifically, the speaking course called 'SpeekUp'.

SpeekUp is a public speaking course created by [David Cook](https://twitter.com/David_Cook). It is run internally at [Telstra Purple](https://www.telstra.com.au/business-enterprise/services/telstra-purple) with the purpose of helping people become better public speakers.

Shuffling Rules:

- Speakers can't introduce themselves
- Speakers can't introduce after speaking

This application was created from the [SAFE template](https://github.com/SAFE-Stack/SAFE-template)

## Prerequisites

### Fake

You'll need [FAKE](https://fake.build/) to build the solution. Install using
`dotnet`

```bash
dotnet tool install fake-cli -g
```

## Work with the application

To concurrently run the server and the client components in watch mode use the following command:

```bash
fake build -t Run
```

## Run tests

```bash
dotnet run --project .\src\Tests\
```
