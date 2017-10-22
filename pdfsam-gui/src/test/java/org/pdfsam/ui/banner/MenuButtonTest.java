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
package org.pdfsam.ui.banner;

import static org.mockito.Matchers.any;
import static org.mockito.Matchers.eq;
import static org.mockito.Mockito.verify;

import org.junit.After;
import org.junit.Test;
import org.loadui.testfx.GuiTest;
import org.loadui.testfx.utils.FXTestUtils;
import org.sejda.injector.Injector;

import javafx.geometry.Side;
import javafx.scene.Parent;

/**
 * @author Andrea Vacondio
 *
 */

public class MenuButtonTest extends GuiTest {

    private Injector injector;

    @Override
    protected Parent getRootNode() {
        injector = Injector.start(new MenuConfig());
        return injector.instance(MenuButton.class);
    }

    @After
    public void tearDown() throws Exception {
        AppContextMenu menu = injector.instance(AppContextMenu.class);
        FXTestUtils.invokeAndWait(() -> menu.hide(), 1);
    }

    @Test
    public void onClick() {
        AppContextMenu menu = injector.instance(AppContextMenu.class);
        click(".button");
        verify(menu).show(any(), eq(Side.BOTTOM), eq(0d), eq(0d));
    }
}
