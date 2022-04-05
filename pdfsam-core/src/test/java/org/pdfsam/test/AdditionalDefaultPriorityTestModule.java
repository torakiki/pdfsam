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
import org.pdfsam.module.Module;
import org.pdfsam.module.ModuleCategory;
import org.pdfsam.module.ModuleDescriptor;
import org.pdfsam.module.ModuleInputOutputType;
import org.pdfsam.module.ModulePriority;

import java.util.Map;

import static org.pdfsam.module.ModuleDescriptorBuilder.builder;

/**
 * @author Andrea Vacondio
 */
public class AdditionalDefaultPriorityTestModule implements Module {

    public static final String ID = "additional.test.module";

    private ModuleDescriptor descriptor = builder().category(ModuleCategory.MERGE).description("Test module")
                                                   .inputTypes(ModuleInputOutputType.SINGLE_PDF).name("ATestModule").priority(ModulePriority.DEFAULT)
                                                   .supportURL("http://www.chucknorrisfacts.com/").build();

    @Override
    public String id() {
        return ID;
    }

    @Override
    public ModuleDescriptor descriptor() {
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
