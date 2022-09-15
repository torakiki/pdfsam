package org.pdfsam.test;
/*
 * This file is part of the PDF Black project
 * Created on 05/04/22
 * Copyright 2022 by Sober Lemur S.a.s di Vacondio Andrea (info@soberlemur.com).
 *
 * You are not permitted to distribute it in any form unless explicit
 * consent is given by Sober Lemur S.a.s di Vacondio Andrea.
 * You are not permitted to modify it.
 *
 * PDF Black is distributed WITHOUT ANY WARRANTY;
 * without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.
 */

import javafx.scene.Node;
import javafx.scene.layout.Pane;
import org.pdfsam.module.Tool;
import org.pdfsam.module.ToolCategory;
import org.pdfsam.module.ToolDescriptor;
import org.pdfsam.module.ToolInputOutputType;
import org.pdfsam.module.ToolPriority;

import java.util.Map;

import static org.pdfsam.module.ToolDescriptorBuilder.builder;

/**
 * @author Andrea Vacondio
 */
public class AdditionalDefaultPriorityTestTool implements Tool {

    public static final String ID = "additional.test.module";

    private ToolDescriptor descriptor = builder().category(ToolCategory.MERGE).description("Test module")
            .inputTypes(ToolInputOutputType.SINGLE_PDF).name("ATestModule").priority(ToolPriority.DEFAULT)
            .supportURL("http://www.chucknorrisfacts.com/")
            .build();

    @Override
    public String id() {
        return ID;
    }

    @Override
    public ToolDescriptor descriptor() {
        return descriptor;
    }

    @Override
    public Pane modulePanel() {
        return null;
    }

    @Override
    public Node graphic() {
        return null;
    }

    @Override
    public void onSaveWorkspace(Map<String, String> data) {
        // nothing
    }

    @Override
    public void onLoadWorkspace(Map<String, String> data) {
        // nothing
    }
}
