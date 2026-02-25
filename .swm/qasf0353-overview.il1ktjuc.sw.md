---
title: QASF0353 Overview
---
# Introduction

This document explains the key design decisions and implementation details behind the QASF0353 form code. We will cover:

1. How user interactions trigger specific UI behaviors like calendar display and LOV (List of Values) invocation.
2. The commit logic and how the system handles saving changes, including special handling for the NPP system.
3. The query used to populate a radio group with filtered NPP records based on roles and employment status.
4. The use of window activation triggers to initialize or refresh form state.

# user interaction triggers for calendar and LOV

The form uses mouse double-click triggers to enhance user experience by providing quick access to auxiliary UI components. For date fields, a double-click calls the calendar widget, allowing users to pick dates visually rather than typing them manually. This is implemented in the WHEN-MOUSE-DOUBLECLICK trigger on the date item.

Similarly, for text items that have an associated LOV, a double-click triggers the LOV display. This is checked dynamically by verifying the item type and LOV property before invoking the LOV list. This approach avoids hardcoding LOV calls and makes the form more flexible to changes in item properties.

<SwmSnippet path="/SampleFiles/qasf0353.txt" line="1">

---

These triggers improve data entry efficiency by reducing manual input errors and speeding up selection.

```
 FOR EACH ROW
 00r88g75b88
 00r100g88b88 00r75g100b0 00r88g100b0
 00r100g100b0 00r0g100b50
 00r25g100b50@
 00r50g100b50
 00r75g100b50
 00r88g100b50 00r100g100b50 00r0g100b75
 00r25g100b75@
 00r50g100b75
 00r75g100b75
 00r88g100b75 00r100g100b75 00r0g100b88
 00r25g100b88@
 00r50g100b88
 00r75g100b88
 00r88g100b88 00r100g100b88
 00r0g100b100 00r25g100b100 00r50g100b100 00r75g100b100 00r88g100b100 00r100g100b100 VC:\Users\VVENKP1\OneDrive - Pearson PLC\Documents\Documents\IQS_FORMS_12C\qasf0353.fmb
 MCopyright (c) 1990, 2019, Oracle and/or its affiliates.  All rights reserved. P49_2026_02_11_13_30_11 BEGIN
 @-- this trigger calls the calendar for a date item
    calendar;
 (WHEN-MOUSE-DOUBLECLICK (C_DATETIME_ITEM) /Lreference CALENDAR
```

---

</SwmSnippet>

<SwmSnippet path="/SampleFiles/qasf0353.txt" line="55">

---

&nbsp;

```
 LOV_MARKER_VA LOV_MARKER_VA P3_2026_02_11_13_30_11 BEGIN
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
 ICON_LEAVE CG$BLOCK_SCROLL_BAR CG$BLOCK_SCROLL_BAR CG$BLOCK_SCROLL_BAR CG$DISPLAY_ITEM CG$DISPLAY_ITEM CG$DISPLAY_ITEM CG$RADIO_TITLE CG$RADIO_TITLE CG$RADIO_TITLE CG$GROUP_TITLE CG$GROUP_TITLE P2_2026_02_11_13_30_11 BEGIN
 when_window_is_activated;
 WHEN-WINDOW-ACTIVATED (Form) $/Lreference WHEN_WINDOW_IS_ACTIVATED
```

---

</SwmSnippet>

# commit logic and NPP system special handling

The commit process is controlled by a key commit trigger that first checks if the form status is 'CHANGED'. If so, it commits the form data. After committing, it checks if the current form belongs to the NPP system (identified by form name prefix). For NPP forms, it displays an alert box confirming the commit and shows a commit message. If no changes were made, it still shows an alert indicating no changes to write.

<SwmSnippet path="/SampleFiles/qasf0353.txt" line="22">

---

This special handling for NPP forms is likely due to external system requirements or user expectations for explicit commit feedback. The commit logic also forces navigation to the next and previous fields to ensure form state is updated before commit.

```
plsql_optimize_level"plsql_code_type"plsql_debug"nls_length_semantics"plsql_warnings"plsql_ccflags"plscope_settings"
 2"INTERPRETED"TRUE"BYTE"BYTE""IDENTIFIERS:NONE" "__anonymous_block"
 P1_2026_02_11_13_30_11 BEGIN
 -- display a message to indicate the records have been saved
 -- force status to be set
    next_field;
    previous_field;
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
 Your changes have been written to the database. No changes to write to the database
 "__anonymous_block" C_RADIO_GROUP
 select npp_id, npp_name 
 from   npps 
 where  npp_id in ( select nrol_npp_id 
                    from   npp_roles 
                    where  nrol_employment_status = 'CU' 
```

---

</SwmSnippet>

# filtering NPP records for radio group population

The radio group is populated by a query that selects NPP IDs and names filtered by roles and employment status. It only includes NPPs with employment status 'CU' (current) and roles that have a positive max centers value. The role ID is dynamically bound from the form context.

<SwmSnippet path="/SampleFiles/qasf0353.txt" line="52">

---

This filtering ensures that only relevant and active NPP records appear in the radio group, improving data accuracy and user selection relevance.

```
                    and    nvl(nrol_max_centres,0) > 0
                    and    nrol_nrty_id            = :CCEN.NBT_ROLE )
 order by npp_surname, npp_initials, npp_id
```

---

</SwmSnippet>

# window activation trigger for form initialization

The form uses a WHEN-WINDOW-ACTIVATED trigger to call a procedure named when_window_is_activated. This is a common pattern to initialize or refresh form data and UI elements whenever the window gains focus or is opened.

<SwmSnippet path="/SampleFiles/qasf0353.txt" line="22">

---

This ensures the form state is consistent and up to date, especially if underlying data might have changed while the form was inactive.

```
plsql_optimize_level"plsql_code_type"plsql_debug"nls_length_semantics"plsql_warnings"plsql_ccflags"plscope_settings"
```

---

</SwmSnippet>

<SwmMeta version="3.0.0" repo-id="Z2l0aHViJTNBJTNBT3JhY2xlLXNhbXBsZSUzQSUzQW11ZGFzaW4x" repo-name="Oracle-sample"><sup>Powered by [Swimm](https://app.swimm.io/)</sup></SwmMeta>
