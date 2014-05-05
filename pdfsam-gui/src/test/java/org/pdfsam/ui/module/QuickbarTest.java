/* 
 * This file is part of the PDF Split And Merge source code
 * Created on 11/feb/2014
 * Copyright 2013 by Andrea Vacondio (andrea.vacondio@gmail.com).
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
package org.pdfsam.ui.module;

import static org.loadui.testfx.Assertions.verifyThat;
import javafx.scene.Parent;
import javafx.scene.control.ContentDisplay;

import org.junit.Ignore;
import org.junit.Test;
import org.loadui.testfx.GuiTest;
import org.pdfsam.ui.workarea.ModuleButton;
import org.pdfsam.ui.workarea.QuickbarWrokarea;
import org.springframework.context.annotation.AnnotationConfigApplicationContext;

/**
 * @author Andrea Vacondio
 */
@Ignore
public class QuickbarTest extends GuiTest {

    private AnnotationConfigApplicationContext ctx;

    @Override
    protected Parent getRootNode() {
        ctx = new AnnotationConfigApplicationContext();
        ctx.register(QuickbarTestConfig.class);
        ctx.refresh();
        return ctx.getBean(QuickbarWrokarea.class);
    }

    @Test
    public void expand() {
        verifyThat(".quickbar-navigation-button",
                (ModuleButton n) -> n.getContentDisplay() == ContentDisplay.GRAPHIC_ONLY);
        click(".quickbar-expand-toggle");
        verifyThat(".quickbar-navigation-button", (ModuleButton n) -> n.getContentDisplay() == ContentDisplay.LEFT);
    }
}
