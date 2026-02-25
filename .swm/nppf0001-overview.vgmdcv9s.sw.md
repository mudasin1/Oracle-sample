---
title: NPPF0001 Overview
---
# Introduction

This document walks through the main logic and design decisions in <SwmPath>[SampleFiles/NPPF0001.txt](/SampleFiles/NPPF0001.txt)</SwmPath>. The code is a mix of PL/SQL triggers and metadata for a legacy Oracle Forms application. The focus is on user interaction, data commit, error handling, and field attribute setup.

We will cover:

1. Why commit logic and user feedback are handled the way they are.
2. How double-click and list-of-values (LOV) logic is implemented.
3. How error handling is structured.
4. Why and how record and field attributes are set up at block entry.
5. The rationale for the verbose field mapping sections.

---

# Commit logic and user feedback

<SwmSnippet path="/SampleFiles/NPPF0001.txt" line="1">

---

The commit trigger is designed to ensure that users are always notified about the result of their save action. The NPP system specifically requires an alert box after a commit, even if there are no changes. This is enforced by checking the form status and displaying the appropriate message. The code also forces the status to be set by moving focus between fields before checking for changes.

```
 00r88g75b88
 00r100g88b88 "__anonymous_block"
 P1_2025_10_28_08_39_34 BEGIN
 -- display a message to indicate the records have been saved
 -- force status to be set
    next_field;
    previous_field;
 IF :System.Form_Status = 'CHANGED' THEN 
     Commit_Form; 
 --  the NPP system requires an alert box for commit messages - don't know why!
     if get_application_property(current_form_name) LIKE 'xxx%' then
        msg_alert('Your changes have been written to the database.','I',false);
        commit_message;
     end if;
 else
 --  the NPP system requires an alert box for commit messages - don't know why!
     if get_application_property(current_form_name) LIKE 'xxx%' then
        msg_alert('No changes to write to the database','I',false);
        message('No changes to write to the database');
     end if;
 KEY-COMMIT (Form) /Lreference MSG_ALERT /Lreference COMMIT_MESSAGE "<anonymous>":System.Form_Status""
 plsql_optimize_level"plsql_code_type"plsql_debug"nls_length_semantics"plsql_warnings"plsql_ccflags"plscope_settings"
 2"INTERPRETED"TRUE"BYTE"BYTE""IDENTIFIERS:NONE"
 Your changes have been written to the database. No changes to write to the database
 "__anonymous_block"
 NRCO_CANVAS NRCO_CANVAS NRCO_WINDOW
 P3_2025_10_28_08_39_34 BEGIN
 :/* This trigger is used to display the list of values when the mouse
    is doubled clicked on a field that has a LOV associated with it.
 if get_item_property(:system.cursor_item,ITEM_TYPE) = 'TEXT ITEM' then
```

---

</SwmSnippet>

---

# Double-click and LOV logic

<SwmSnippet path="/SampleFiles/NPPF0001.txt" line="31">

---

To improve usability, the code checks if a double-clicked field is a text item and has an associated LOV. If so, it triggers the LOV popup. This avoids unnecessary popups on fields that don't support LOVs.

```
    if get_item_property(:system.cursor_item,LIST) = 'TRUE' then
       list_values;
    end if;
```

---

</SwmSnippet>

---

# Error handling

<SwmSnippet path="/SampleFiles/NPPF0001.txt" line="34">

---

Error handling is centralized in a trigger that checks for a specific error code (40401). If this code is encountered, it is trapped and handled by a custom procedure. This prevents unhandled errors from disrupting the user experience and allows for targeted error responses.

```
 WHEN-MOUSE-DOUBLECLICK (Form) "<anonymous>":system.cursor_item""
 plsql_optimize_level"plsql_code_type"plsql_debug"nls_length_semantics"plsql_warnings"plsql_ccflags"plscope_settings"
 2"INTERPRETED"TRUE"BYTE"BYTE""IDENTIFIERS:NONE"
 "__anonymous_block"
 NRWA_CANVAS NRWA_CANVAS NRWA_WINDOW
 transparent
 TEXTSTR439 TEXTSTR439 WORK ALLOCATION
 P0_2025_10_28_08_39_34 BEGIN
 declare
  ERR_VAL NUMBER(5)     := ERROR_CODE;
 begin
  IF ERR_VAL=40401 THEN
       null;
     trap_error_code;
   END IF;
 ON-ERROR (Form) /Lreference TRAP_ERROR_CODE
 plsql_optimize_level"plsql_code_type"plsql_debug"nls_length_semantics"plsql_warnings"plsql_ccflags"plscope_settings"
 2"INTERPRETED"TRUE"BYTE"BYTE""IDENTIFIERS:NONE" "__anonymous_block"
 FALSE NPP2 "__anonymous_block"
 qFRM50_BUFFER table contains user's buffer ,5_ROSATTRIBS class attributes )thROSLFDESC ros lf descrip
 'NDROSOBJMAP ros name->id ROSSTRINGS tk2 uiStrings ROSSTRUCTS struct typesÍ« ROSTK2RESV tk2 reserved TOOL_MODULE table contains information about different module VG_COLOR VG_COLOR )VG_COLOR ITEMID )VG_COLOR CELLID +VG_COLOR NAME_SET -VG_COLOR NAME_LENGTH -VG_COLOR COLOR_NAME 'VG_COLOR GREEN 'VG_COLOR BLUE "__anonymous_block"
 00r88g75b88
 00r100g88b88 "__anonymous_block" P2_2025_10_28_08_39_34 BEGIN
 when_window_is_activated;
 WHEN-WINDOW-ACTIVATED (Form) $/Lreference WHEN_WINDOW_IS_ACTIVATED
```

---

</SwmSnippet>

---

# Record and field attribute setup

<SwmSnippet path="/SampleFiles/NPPF0001.txt" line="59">

---

On entering a block, the code sets up display attributes for the current record and checks if the block is query-only. This ensures that the UI reflects the correct state for editing or viewing data, and that any restrictions are enforced immediately.

```
plsql_optimize_level"plsql_code_type"plsql_debug"nls_length_semantics"plsql_warnings"plsql_ccflags"plscope_settings"
 2"INTERPRETED"TRUE"BYTE"BYTE""IDENTIFIERS:NONE" "__anonymous_block" "__anonymous_block"
 FALSE NPP2 "__anonymous_block"
 P4_2025_10_28_08_39_34 BEGIN
 xbegin
 /* sets the display type for current records. */
    set_current_record_attribute;
    check_if_query_only;
 PRE-BLOCK (Form) (/Lreference SET_CURRENT_RECORD_ATTRIBUTE
 P241_2025_10_28_08_39_34
 NPP2.NPP_INITIALS NPP2.NPP_INITIALS
 NPP_INITIALS
 P241_2025_10_28_08_39_34
 NPP2.NPP_FORENAMES NPP2.NPP_FORENAMES
 NPP_FORENAMES
 P241_2025_10_28_08_39_34
 NPP2.NPP_GENDER NPP2.NPP_GENDER
 NPP_GENDER
 P241_2025_10_28_08_39_34
 NPP2.NPP_DATE_OF_BIRTH NPP2.NPP_DATE_OF_BIRTH
 NPP_DATE_OF_BIRTH
 P241_2025_10_28_08_39_34
 NPP2.NBT_NADD_POSTCODE NPP2.NBT_NADD_POSTCODE
 NBT_NADD_POSTCODE
 P244_2025_10_28_08_39_34
 NAWA.NAWA_UPDATE_DATE NAWA.NAWA_UPDATE_DATE
 NAWA_UPDATE_DATE
 P248_2025_10_28_08_39_34
 NAWA.NAWA_UPDATE_USER NAWA.NAWA_UPDATE_USER
 NAWA_UPDATE_USER
```

---

</SwmSnippet>

---

# Field mapping and metadata

<SwmSnippet path="/SampleFiles/NPPF0001.txt" line="89">

---

The long sections mapping database fields to form fields are necessary for Oracle Forms to bind data correctly. These mappings are verbose but required for the form to function, as they define how data is loaded, displayed, and saved for each record.

```
 P248_2025_10_28_08_39_34
 NAWA.NAWA_NPP_ID NAWA.NAWA_NPP_ID
 NAWA_NPP_ID
 P249_2025_10_28_08_39_34
 NAWA.NAWA_AT_NUMBER NAWA.NAWA_AT_NUMBER
 NAWA_AT_NUMBER
 P249_2025_10_28_08_39_34 P250_2025_10_28_08_39_34 P251_2025_10_28_08_39_34
 NAWA.NAWA_AC_CODE NAWA.NAWA_AC_CODE
 NAWA_AC_CODE
 P249_2025_10_28_08_39_34 P250_2025_10_28_08_39_34 P251_2025_10_28_08_39_34
 NAWA.NAWA_INSERT_DATE NAWA.NAWA_INSERT_DATE
 NAWA_INSERT_DATE
 P249_2025_10_28_08_39_34
 NAWA.NAWA_INSERT_USER NAWA.NAWA_INSERT_USER
 NAWA_INSERT_USER
 P249_2025_10_28_08_39_34
 NAWA.NBT_AT_NAME NAWA.NBT_AT_NAME
 NBT_AT_NAME
 P250_2025_10_28_08_39_34 P251_2025_10_28_08_39_34
 NAWA.NBT_AC_DESCRIPTION NAWA.NBT_AC_DESCRIPTION
 NBT_AC_DESCRIPTION
 P250_2025_10_28_08_39_34 P251_2025_10_28_08_39_34
 NAWA.NBT_EXTASS NAWA.NBT_EXTASS
 NBT_EXTASS
 P250_2025_10_28_08_39_34 P251_2025_10_28_08_39_34
 NRAV.NRAV_UPDATE_DATE NRAV.NRAV_UPDATE_DATE
 NRAV_UPDATE_DATE
 P252_2025_10_28_08_39_34
 NRAV.NRAV_UPDATE_USER NRAV.NRAV_UPDATE_USER
 NRAV_UPDATE_USER
```

---

</SwmSnippet>

---

# Summary

- Commit logic is explicit to meet NPP system requirements for user feedback.
- Double-click triggers are guarded to only show LOVs when appropriate.
- Error handling is centralized and traps specific codes.
- Block entry logic ensures correct UI state.
- Field mappings are verbose but required for Oracle Forms data binding.

This structure is dictated by Oracle Forms' limitations and the need for explicit user feedback and error handling in the NPP system.

<SwmMeta version="3.0.0" repo-id="Z2l0aHViJTNBJTNBT3JhY2xlLXNhbXBsZSUzQSUzQW11ZGFzaW4x" repo-name="Oracle-sample"><sup>Powered by [Swimm](https://app.swimm.io/)</sup></SwmMeta>
