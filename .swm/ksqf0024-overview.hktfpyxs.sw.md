---
title: KSQF0024 Overview
---
# Introduction

This document explains the key design decisions and implementation details behind the code in <SwmPath>[SampleFiles/ksqf0024.txt](/SampleFiles/ksqf0024.txt)</SwmPath>. We will cover:

1. How error handling is integrated at the row level.
2. The approach to user interaction, especially commit behavior and alerts.
3. The handling of mouse double-click events to trigger specific UI actions.
4. The use of LOV (List of Values) and how it is displayed on user interaction.

# error handling at the row level

<SwmSnippet path="/SampleFiles/ksqf0024.txt" line="1">

---

The code uses a FOR EACH ROW trigger to catch errors during database operations. This is done by trapping a specific error code and referencing a custom error handler. This approach ensures that any row-level exceptions are caught and managed explicitly, preventing unhandled exceptions from propagating and allowing for controlled error reporting or recovery.

```
 FOR EACH ROW
  (ERROR (F 00r88g75b88
 00r100g88b88 /Lreference TRAP_ERROR_CODE
```

---

</SwmSnippet>

# commit logic and user feedback

The commit logic is designed to check the form's status before committing changes. If the form has changes, it commits the form and then, for forms matching a certain naming pattern (those starting with "NPP"), it displays an alert box confirming the commit. If there are no changes, it similarly shows an alert indicating no changes were written. This conditional alerting is likely to meet specific system requirements for user feedback in the NPP system.

<SwmSnippet path="/SampleFiles/ksqf0024.txt" line="4">

---

The code also disables a specific item ('SARE.NBT_ATAR_DESC') after commit, which suggests a UI state change to prevent further editing or to reflect the committed state.

```
plsql_optimize_level"plsql_code_type"plsql_debug"nls_length_semantics"plsql_warnings"plsql_ccflags"plscope_settings"
 2"INTERPRETED"TRUE"BYTE"BYTE""IDENTIFIERS:NONE" "__anonymous_block" 00r100g100b88
 00r0g100b100 00r25g100b100 00r50g100b100 00r75g100b100 00r88g100b100 00r100g100b100 VC:\Users\VVENKP1\OneDrive - Pearson PLC\Documents\Documents\IQS_FORMS_12C\ksqf0024.fmb
 MCopyright (c) 1990, 2019, Oracle and/or its affiliates.  All rights reserved. P49_2025_08_05_16_59_26 BEGIN
 @-- this trigger calls the calendar for a date item
    calendar;
 (WHEN-MOUSE-DOUBLECLICK (C_DATETIME_ITEM) /Lreference CALENDAR
plsql_optimize_level"plsql_code_type"plsql_debug"nls_length_semantics"plsql_warnings"plsql_ccflags"plscope_settings"
 2"INTERPRETED"TRUE"BYTE"BYTE""IDENTIFIERS:NONE" "__anonymous_block" P1_2025_08_05_16_59_26 BEGIN
 7-- display a message to indicate the records have been saved
 -- force status to be set
    next_field;
    previous_field;
    SET_ITEM_PROPERTY('SARE.NBT_ATAR_DESC',ENABLED,PROPERTY_FALSE);
 IF :System.Form_Status = 'CHANGED' THEN 
     Commit_Form; 
 --  the NPP system requires an alert box for commit messages - don't know why!
     if get_application_property(current_form_name) LIKE 'NPP%' then
        msg_alert('Your changes have been written to the database.','I',false);
        commit_message;
     end if;
 else
 --  the NPP system requires an alert box for commit messages - don't know why!
     if get_application_property(current_form_name) LIKE 'NPP%' then
        msg_alert('No changes to write to the database','I',false);
        message('No changes to write to the database');
     end if;
 KEY-COMMIT (Form) /Lreference MSG_ALERT /Lreference COMMIT_MESSAGE "<anonymous>":System.Form_Status""
 plsql_optimize_level"plsql_code_type"plsql_debug"nls_length_semantics"plsql_warnings"plsql_ccflags"plscope_settings"
 2"INTERPRETED"TRUE"BYTE"BYTE""IDENTIFIERS:NONE"
```

---

</SwmSnippet>

# mouse double-click event handling

The code listens for mouse double-click events on date/time fields and triggers a calendar popup. This improves user experience by providing a quick way to select dates without manual entry.

<SwmSnippet path="/SampleFiles/ksqf0024.txt" line="34">

---

Additionally, there is a more generic double-click handler that checks if the current item is a text item with an associated LOV. If so, it triggers the display of the LOV list. This dynamic behavior allows users to access value lists contextually, enhancing data entry efficiency.

```
 SARE.NBT_ATAR_DESC Your changes have been written to the database. No changes to write to the database
 "__anonymous_block" WHEN-MOUSE-DOUBLECLICK WHEN-MOUSE-DOUBLECLICK C_DATETIME_ITEM C_DATETIME_ITEM C_DATETIME_ITEM WHEN-MOUSE-DOUBLECLICK WHEN-MOUSE-DOUBLECLICK
 C_AMOUNT_ITEM C_AMOUNT_ITEM
 C_AMOUNT_ITEM C_SHORT_TEXT_ITEM P3_2025_08_05_16_59_26 BEGIN
 :/* This trigger is used to display the list of values when the mouse
    is doubled clicked on a field that has a LOV associated with it.
 if get_item_property(:system.cursor_item,ITEM_TYPE) = 'TEXT ITEM' then
    if get_item_property(:system.cursor_item,LIST) = 'TRUE' then
       list_values;
    end if;
 WHEN-MOUSE-DOUBLECLICK (Form) "<anonymous>":system.cursor_item""
 plsql_optimize_level"plsql_code_type"plsql_debug"nls_length_semantics"plsql_warnings"plsql_ccflags"plscope_settings"
 2"INTERPRETED"TRUE"BYTE"BYTE""IDENTIFIERS:NONE"
 "__anonymous_block"
 C_POP_LIST_ITEM C_COMBO_LIST_ITEM C_COMBO_LIST_ITEM C_COMBO_LIST_ITEM C_CHECK_BOX C_CHECK_BOX C_CHECK_BOX C_RADIO_BUTTON C_RADIO_BUTTON C_RADIO_BUTTON C_RADIO_GROUP C_RADIO_GROUP C_RADIO_GROUP
 select to_char(atar_percentage)||'%',atar_desc,atar_id from assessment_tariffs where atar_delete_ind = 'N' order by atar_percentage,atar_desc
 P2_2025_08_05_16_59_26 BEGIN
 when_window_is_activated;
 WHEN-WINDOW-ACTIVATED (Form) $/Lreference WHEN_WINDOW_IS_ACTIVATED
```

---

</SwmSnippet>

# summary

The implementation focuses on robust error handling at the database row level, clear user feedback on commit actions tailored to specific forms, and intuitive UI interactions through mouse events. These design choices aim to improve reliability, user awareness, and ease of data entry without adding unnecessary complexity.

<SwmMeta version="3.0.0" repo-id="Z2l0aHViJTNBJTNBT3JhY2xlLXNhbXBsZSUzQSUzQW11ZGFzaW4x" repo-name="Oracle-sample"><sup>Powered by [Swimm](https://app.swimm.io/)</sup></SwmMeta>
