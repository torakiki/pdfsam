/* 
 * This file is part of the PDF Split And Merge source code
 * Created on 27/ago/2014
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
import javafx.scene.Node;
import javafx.scene.layout.Pane;

import org.pdfsam.module.Module;
import org.pdfsam.module.ModuleCategory;
import org.pdfsam.module.ModuleDescriptor;
import org.pdfsam.module.ModulePriority;
import org.pdfsam.ui.workspace.SaveWorkspaceEvent;

/**
 * A test module to use in unit tests
 * 
 * @author Andrea Vacondio
 *
 */
public class LowPriorityTestModule implements Module {
    public static final String ID = "low.test.module";

    private ModuleDescriptor descriptor = builder().category(ModuleCategory.SPLIT)
            .description("Low priority test module").name("LowPriorityTestModule").priority(ModulePriority.LOW).build();

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

    public void onSaveWorkspace(SaveWorkspaceEvent event) {
        // nothing
    }
}
