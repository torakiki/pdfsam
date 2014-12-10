/* 
 * This file is part of the PDF Split And Merge source code
 * Created on 20/ago/2014
 * Copyright 2013-2014 by Andrea Vacondio (andrea.vacondio@gmail.com).
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

import javafx.scene.Node;
import javafx.scene.layout.Pane;

import org.pdfsam.module.Module;
import org.pdfsam.module.ModuleCategory;
import org.pdfsam.module.ModuleDescriptor;
import org.pdfsam.module.ModulePriority;

/**
 * A test module to use in tests
 * 
 * @author Andrea Vacondio
 *
 */
public class DefaultPriorityTestModule implements Module {
    public static final String ID = "test.module";

    private ModuleDescriptor descriptor = builder().category(ModuleCategory.MERGE).description("Test module")
            .name("TestModule").priority(ModulePriority.DEFAULT).supportURL("http://www.chucknorrisfacts.com/").build();

    public String id() {
        return ID;
    }

    public ModuleDescriptor descriptor() {
        return descriptor;
    }

    public Pane modulePanel() {
        return null;
    }

    public Node graphic() {
        return null;
    }

    public void onSaveWorkspace(Map<String, String> data) {
        // nothing
    }

    public void onLoadWorkspace(Map<String, String> data) {
        // nothing
    }

}
