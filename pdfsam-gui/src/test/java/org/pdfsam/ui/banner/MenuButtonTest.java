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
package org.pdfsam.ui.banner;

import static org.mockito.Matchers.any;
import static org.mockito.Matchers.eq;
import static org.mockito.Mockito.verify;

import javax.inject.Inject;

import org.junit.After;
import org.junit.Test;
import org.junit.experimental.categories.Category;
import org.junit.runner.RunWith;
import org.loadui.testfx.GuiTest;
import org.loadui.testfx.categories.TestFX;
import org.loadui.testfx.utils.FXTestUtils;
import org.springframework.context.ApplicationContext;
import org.springframework.test.annotation.DirtiesContext;
import org.springframework.test.annotation.DirtiesContext.ClassMode;
import org.springframework.test.context.ContextConfiguration;
import org.springframework.test.context.junit4.SpringJUnit4ClassRunner;

import javafx.geometry.Side;
import javafx.scene.Parent;

/**
 * @author Andrea Vacondio
 *
 */
@Category(TestFX.class)
@RunWith(SpringJUnit4ClassRunner.class)
@ContextConfiguration(classes = { MenuConfig.class })
@DirtiesContext(classMode = ClassMode.AFTER_CLASS)
public class MenuButtonTest extends GuiTest {

    @Inject
    private ApplicationContext applicationContext;

    @Override
    protected Parent getRootNode() {
        return applicationContext.getBean(MenuButton.class);
    }

    @After
    public void tearDown() throws Exception {
        AppContextMenu menu = applicationContext.getBean(AppContextMenu.class);
        FXTestUtils.invokeAndWait(() -> menu.hide(), 1);
    }

    @Test
    public void onClick() {
        AppContextMenu menu = applicationContext.getBean(AppContextMenu.class);
        click(".button");
        verify(menu).show(any(), eq(Side.BOTTOM), eq(0d), eq(0d));
    }
}
