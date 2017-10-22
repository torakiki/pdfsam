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
package org.pdfsam.ui.banner;

import static org.junit.Assert.assertEquals;
import static org.mockito.Mockito.mock;
import static org.mockito.Mockito.verify;
import static org.sejda.eventstudio.StaticStudio.eventStudio;

import org.junit.Rule;
import org.junit.Test;
import org.junit.experimental.categories.Category;
import org.loadui.testfx.GuiTest;
import org.loadui.testfx.categories.TestFX;
import org.mockito.ArgumentCaptor;
import org.pdfsam.i18n.DefaultI18nContext;
import org.pdfsam.test.ClearEventStudioRule;
import org.pdfsam.ui.dashboard.PreferencesDashboardItem;
import org.pdfsam.ui.event.SetActiveDashboardItemRequest;
import org.sejda.eventstudio.Listener;
import org.sejda.injector.Injector;

import javafx.scene.Parent;

/**
 * @author Andrea Vacondio
 *
 */
@Category(TestFX.class)
public class SettingsMenuTest extends GuiTest {

    @Rule
    public ClearEventStudioRule clearStudio = new ClearEventStudioRule();
    private Injector injector;

    @Override
    protected Parent getRootNode() {
        injector = Injector.start(new MenuConfig());
        return injector.instance(MenuButton.class);
    }

    @Test
    public void onSettingsClick() {
        Listener<SetActiveDashboardItemRequest> listener = mock(Listener.class);
        eventStudio().add(SetActiveDashboardItemRequest.class, listener);
        ArgumentCaptor<SetActiveDashboardItemRequest> argument = ArgumentCaptor
                .forClass(SetActiveDashboardItemRequest.class);
        click(".button").click(DefaultI18nContext.getInstance().i18n("_Settings"));
        verify(listener).onEvent(argument.capture());
        assertEquals(PreferencesDashboardItem.ID, argument.getValue().getActiveItemId());
    }
}
