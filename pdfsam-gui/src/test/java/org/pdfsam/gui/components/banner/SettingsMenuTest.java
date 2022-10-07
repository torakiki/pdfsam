/*
 * This file is part of the PDF Split And Merge source code
 * Created on 02 mag 2017
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
package org.pdfsam.gui.components.banner;

import javafx.scene.Scene;
import javafx.stage.Stage;
import org.junit.jupiter.api.Tag;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.extension.ExtendWith;
import org.mockito.ArgumentCaptor;
import org.pdfsam.eventstudio.Listener;
import org.pdfsam.gui.components.dashboard.PreferencesDashboardItem;
import org.pdfsam.injector.Injector;
import org.pdfsam.model.ui.SetActiveDashboardItemRequest;
import org.pdfsam.test.ClearEventStudioExtension;
import org.testfx.api.FxRobot;
import org.testfx.framework.junit5.ApplicationExtension;
import org.testfx.framework.junit5.Start;

import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.mockito.Mockito.mock;
import static org.mockito.Mockito.verify;
import static org.pdfsam.eventstudio.StaticStudio.eventStudio;
import static org.pdfsam.i18n.I18nContext.i18n;

/**
 * @author Andrea Vacondio
 */
@ExtendWith({ ApplicationExtension.class, ClearEventStudioExtension.class })
public class SettingsMenuTest {

    private Injector injector;
    private FxRobot robot;

    @Start
    public void start(Stage stage) {
        injector = Injector.start(new MenuConfig());
        Scene scene = new Scene(injector.instance(MenuButton.class));
        stage.setScene(scene);
        stage.show();
    }

    @Test
    @Tag("NoHeadless")
    public void onSettingsClick() {
        Listener<SetActiveDashboardItemRequest> listener = mock(Listener.class);
        eventStudio().add(SetActiveDashboardItemRequest.class, listener);
        ArgumentCaptor<SetActiveDashboardItemRequest> argument = ArgumentCaptor.forClass(
                SetActiveDashboardItemRequest.class);
        robot.clickOn(".button").clickOn(i18n().tr("_Settings"));
        verify(listener).onEvent(argument.capture());
        assertEquals(PreferencesDashboardItem.ID, argument.getValue().id());
    }
}
