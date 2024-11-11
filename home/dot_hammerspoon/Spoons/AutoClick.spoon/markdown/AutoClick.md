# [docs](index.md) » AutoClick
---

Autoclicker tool, configurable with clicks per seconds

Download: [https://github.com/Carleslc/Spoons/raw/master/Spoons/AutoClick.spoon.zip](https://github.com/Carleslc/Spoons/raw/master/Spoons/AutoClick.spoon.zip)

## API Overview
* Variables - Configurable values
 * [clicksPerSecond](#clicksPerSecond)
* Methods - API calls which can only be made on an object returned by a constructor
 * [bindHotkeys](#bindHotkeys)
 * [init](#init)
 * [isRunning](#isRunning)
 * [trigger](#trigger)

## API Documentation

### Variables

| [clicksPerSecond](#clicksPerSecond)         |                                                                                     |
| --------------------------------------------|-------------------------------------------------------------------------------------|
| **Signature**                               | `AutoClick.clicksPerSecond`                                                                    |
| **Type**                                    | Variable                                                                     |
| **Description**                             | Clicks per second. May not work properly if set too high (above ~50). Defaults to 10.                                                                     |

### Methods

| [bindHotkeys](#bindHotkeys)         |                                                                                     |
| --------------------------------------------|-------------------------------------------------------------------------------------|
| **Signature**                               | `AutoClick:bindHotkeys(mapping)`                                                                    |
| **Type**                                    | Method                                                                     |
| **Description**                             | Binds hotkeys for AutoClick                                                                     |
| **Parameters**                              | <ul><li>mapping - A table containing hotkey modifier/key details for the following items:</li><li> triggerAutoClick - Start/Stop AutoClick</li></ul> |

| [init](#init)         |                                                                                     |
| --------------------------------------------|-------------------------------------------------------------------------------------|
| **Signature**                               | `AutoClick:init()`                                                                    |
| **Type**                                    | Method                                                                     |
| **Description**                             | Initializes AutoClick                                                                     |
| **Parameters**                              | <ul><li>None</li></ul> |

| [isRunning](#isRunning)         |                                                                                     |
| --------------------------------------------|-------------------------------------------------------------------------------------|
| **Signature**                               | `AutoClick:isRunning()`                                                                    |
| **Type**                                    | Method                                                                     |
| **Description**                             | Checks if this spoon is enabled and running                                                                     |
| **Parameters**                              | <ul><li>None</li></ul> |
| **Returns**                                 | <ul><li>A boolean, true if this spoon is running, otherwise false</li></ul>          |

| [trigger](#trigger)         |                                                                                     |
| --------------------------------------------|-------------------------------------------------------------------------------------|
| **Signature**                               | `AutoClick:trigger()`                                                                    |
| **Type**                                    | Method                                                                     |
| **Description**                             | Start/Stop AutoClick                                                                     |
| **Parameters**                              | <ul><li>None</li></ul> |

