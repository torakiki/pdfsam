/* 
 * This file is part of the PDF Split And Merge source code
 * Created on 20/ago/2014
 * Copyright 2017 by Sober Lemur S.a.s. di Vacondio Andrea (info@pdfsam.org).
 *
 * This program is free software: you can redistribute it and/or modify
 * it under the terms of the GNU Affero General Public License as 
 * published by the Free Software Foundation, either version 3 of the 
 * License, or (at your option) any later version.
 *
 * This program is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU General Public License for more details.
 *
 * You should have received a copy of the GNU Affero General Public License
 * along with this program.  If not, see <http://www.gnu.org/licenses/>.
 */
package org.pdfsam.test;

import static org.pdfsam.module.ModuleDescriptorBuilder.builder;

import java.util.Map;

import org.pdfsam.module.Module;
import org.pdfsam.module.ModuleCategory;
import org.pdfsam.module.ModuleDescriptor;
import org.pdfsam.module.ModuleInputOutputType;
import org.pdfsam.module.ModulePriority;

import javafx.scene.Node;
import javafx.scene.layout.Pane;

/**
 * A test module to use in tests
 * 
 * @author Andrea Vacondio
 *
 */
public class DefaultPriorityTestModule implements Module {
    public static final String ID = "test.module";

    private ModuleDescriptor descriptor = builder().category(ModuleCategory.MERGE).description("Test module")
            .inputTypes(ModuleInputOutputType.SINGLE_PDF).name("TestModule").priority(ModulePriority.DEFAULT)
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
