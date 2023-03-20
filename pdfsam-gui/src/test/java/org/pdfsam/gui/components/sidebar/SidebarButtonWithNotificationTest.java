package org.pdfsam.gui.components.sidebar;

import javafx.scene.paint.Color;
import javafx.scene.shape.Circle;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.extension.ExtendWith;
import org.kordamp.ikonli.boxicons.BoxiconsRegular;
import org.kordamp.ikonli.javafx.FontIcon;
import org.pdfsam.test.ClearEventStudioExtension;
import org.pdfsam.test.JavaFxThreadInitializeExtension;

import static org.assertj.core.api.Assertions.assertThat;
import static org.junit.jupiter.api.Assertions.assertFalse;
import static org.junit.jupiter.api.Assertions.assertTrue;

/*
 * This file is part of the PDF Split And Merge source code
 * Created on 20/01/23
 * Copyright 2023 by Sober Lemur S.r.l. (info@pdfsam.org).
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
@ExtendWith({ ClearEventStudioExtension.class, JavaFxThreadInitializeExtension.class })
class SidebarButtonWithNotificationTest {

    private SidebarButtonWithNotification<SidebarButton> victim;
    private Circle notification;

    @BeforeEach
    public void setUp() {
        this.notification = new Circle(10, Color.DARKKHAKI);
        this.victim = new SidebarButtonWithNotification<>(new SidebarButton("test", new FontIcon(BoxiconsRegular.NEWS)),
                notification, SidebarNotificationType.ERROR);
    }

    @Test
    public void showNotification() {
        assertFalse(notification.isVisible());
        assertThat(victim.getWrapped().getStyleClass()).doesNotContain(SidebarNotificationType.ERROR.getCssClass());
        victim.showNotification();
        assertTrue(notification.isVisible());
        assertThat(victim.getWrapped().getStyleClass()).contains(SidebarNotificationType.ERROR.getCssClass());
    }

    @Test
    public void hideNotification() {
        victim.showNotification();
        assertTrue(notification.isVisible());
        assertThat(victim.getWrapped().getStyleClass()).contains(SidebarNotificationType.ERROR.getCssClass());
        victim.hideNotification();
        assertFalse(notification.isVisible());
        assertThat(victim.getWrapped().getStyleClass()).doesNotContain(SidebarNotificationType.ERROR.getCssClass());
    }

}