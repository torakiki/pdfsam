/* 
 * This file is part of the PDF Split And Merge source code
 * Created on 23/ago/2014
 * Copyright 2017 by Sober Lemur S.a.s. (info@pdfsam.org).
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
package org.pdfsam.ui.notification;

import static org.junit.Assert.assertNotNull;

import org.junit.Rule;
import org.junit.Test;
import org.loadui.testfx.utils.FXTestUtils;
import org.pdfsam.test.InitializeJavaFxThreadRule;

import javafx.scene.control.Label;
import javafx.scene.layout.HBox;

/**
 * @author Andrea Vacondio
 *
 */
public class NotificationsContainerTest {

    @Rule
    public InitializeJavaFxThreadRule javaFxThread = new InitializeJavaFxThreadRule();

    @Test
    public void addNotification() throws Exception {
        NotificationsContainer victim = new NotificationsContainer();
        HBox node = new HBox(new Label("Chuck"));
        node.setId("chuck");
        victim.addNotification("myTitle", node);
        FXTestUtils.invokeAndWait(() -> assertNotNull(victim.lookup("#chuck")), 1);
    }

    @Test
    public void addStickyNotification() throws Exception {
        NotificationsContainer victim = new NotificationsContainer();
        Label node = new Label("Chuck");
        node.setId("chuck");
        victim.addStickyNotification("myTitle", node);
        FXTestUtils.invokeAndWait(() -> assertNotNull(victim.lookup("#chuck")), 1);
    }
}
