/* 
 * This file is part of the PDF Split And Merge source code
 * Created on 21/ago/2014
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

import static org.sejda.eventstudio.StaticStudio.eventStudio;

import java.net.URISyntaxException;

import javax.inject.Named;

import org.junit.Rule;
import org.junit.Test;
import org.junit.experimental.categories.Category;
import org.loadui.testfx.GuiTest;
import org.loadui.testfx.categories.TestFX;
import org.loadui.testfx.utils.FXTestUtils;
import org.pdfsam.test.ClearEventStudioRule;
import org.pdfsam.ui.event.SetTitleEvent;
import org.sejda.injector.Injector;
import org.sejda.injector.Prototype;
import org.sejda.injector.Provides;

import javafx.scene.Parent;
import javafx.scene.image.Image;
import javafx.scene.image.ImageView;

/**
 * @author Andrea Vacondio
 *
 */
@Category(TestFX.class)
public class BannerPaneTest extends GuiTest {
    @Rule
    public ClearEventStudioRule cleanStudio = new ClearEventStudioRule();
    private Injector injector;

    @Override
    protected Parent getRootNode() {
        injector = Injector.start(new MenuConfig(), new Config());
        return injector.instance(BannerPane.class);
    }

    static class Config {
        @Provides
        public BannerPane victim(BannerButtons buttons, @Named("logo32") Image logo, ImageView payoff) {
            return new BannerPane(buttons, payoff, logo);
        }

        @Provides
        @Prototype
        public ImageView payoff() throws URISyntaxException {
            return new ImageView(this.getClass().getResource("/images/payoff.png").toURI().toString());
        }

        @Provides
        @Named("logo32")
        @Prototype
        public Image logo32() {
            return new Image(this.getClass().getResourceAsStream("/images/payoff.png"));
        }

        @Provides
        public BannerButtons buttons(LogButton logButton, DashboardButton dashboardButton, NewsButton newsButton,
                MenuButton menuButton) {
            return new BannerButtons(logButton, dashboardButton, newsButton, menuButton);
        }

        @Provides
        public DashboardButton dashboardButton() {
            return new DashboardButton("itemId");
        }

        @Provides
        public NewsButton newsButton() {
            return new NewsButton();
        }

    }

    @Test
    public void title() throws Exception {
        FXTestUtils.invokeAndWait(() -> eventStudio().broadcast(new SetTitleEvent("title")), 1);
        exists("@title");
    }

}
