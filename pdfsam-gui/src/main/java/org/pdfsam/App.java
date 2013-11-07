/* 
 * This file is part of the PDF Split And Merge source code
 * Created on 14/dic/2011
 * Copyright 2011 by Andrea Vacondio (andrea.vacondio@gmail.com).
 *
 * This program is free software: you can redistribute it and/or modify
 * it under the terms of the GNU General Public License as published by
 * the Free Software Foundation, either version 3 of the License, or
 * (at your option) any later version.
 *
 * This program is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU General Public License for more details.
 *
 * You should have received a copy of the GNU General Public License
 * along with this program.  If not, see <http://www.gnu.org/licenses/>.
 */
package org.pdfsam;

import java.awt.Image;
import java.util.Map;

import javax.swing.ImageIcon;

import org.apache.commons.lang3.time.DurationFormatUtils;
import org.apache.commons.lang3.time.StopWatch;
import org.bushe.swing.event.EventBus;
import org.pdfsam.configuration.ApplicationContextHolder;
import org.pdfsam.context.DefaultI18nContext;
import org.pdfsam.context.DefaultUserContext;
import org.pdfsam.gui.MainFrame;
import org.pdfsam.gui.WelcomePanel;
import org.pdfsam.gui.menu.MenuType;
import org.pdfsam.gui.support.SwingUtils;
import org.pdfsam.module.BaseTaskExecutionModule;
import org.pdfsam.ui.module.TaskExecutionModulesLoadedEvent;
import org.pdfsam.update.UpdateCheckRequest;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import org.springframework.core.env.Environment;

import bibliothek.gui.dock.common.CControl;
import bibliothek.gui.dock.common.theme.ThemeMap;
import bibliothek.gui.dock.util.IconManager;

/**
 * Entry point to start pdfsam
 * 
 * @author Andrea Vacondio
 * 
 */
public final class App {

    private static final Logger LOG = LoggerFactory.getLogger(App.class);
    private static MainFrame mainFrame = null;
    private static final String UNK = "UNKNOWN";

    private App() {
        // hide
    }

    public static void main(String[] args) throws Exception {
        LOG.info(DefaultI18nContext.getInstance().i18n("Starting pdfsam"));
        StopWatch stopWatch = new StopWatch();
        stopWatch.start();

        try {
            initLookAndFeel();
            Environment env = ApplicationContextHolder.getContext().getEnvironment();
            mainFrame = new MainFrame(String.format("PDF Split and Merge %s ver. %s",
                    env.getProperty("pdfsam.package", UNK), env.getProperty("pdfsam.version", UNK)));
            mainFrame.setIconImage(ApplicationContextHolder.getContext().getBean(Image.class));
            CControl control = new CControl(mainFrame);
            control.setTheme(ThemeMap.KEY_FLAT_THEME);
            initIcons(control);
            mainFrame.initControl(control);
            mainFrame.addSystemContentAction(MenuType.HELP, new WelcomePanel());

            initModules();
            SwingUtils.centrePositionOnScreen(mainFrame);
            mainFrame.setVisible(true);
            requestCheckForUpdateIfNecessary();
        } catch (Exception e) {
            LOG.error(DefaultI18nContext.getInstance().i18n("Error starting pdfsam."), e);
            throw e;
        }
        stopWatch.stop();
        LOG.info(DefaultI18nContext.getInstance().i18n("Started in {0}",
                DurationFormatUtils.formatDurationWords(stopWatch.getTime(), true, true)));
    }

    private static void requestCheckForUpdateIfNecessary() {
        if (DefaultUserContext.getInstance().isCheckForUpdates()) {
            EventBus.publish(new UpdateCheckRequest());
        }
    }

    private static void initIcons(CControl control) {
        IconManager icons = control.getController().getIcons();
        icons.setIconClient("locationmanager.normalize",
                new ImageIcon(App.class.getResource("/data/bibliothek/gui/dock/common/icons/eclipse/normalize.png")));
        icons.setIconClient("locationmanager.maximize",
                new ImageIcon(App.class.getResource("/data/bibliothek/gui/dock/common/icons/eclipse/maximize.png")));
        icons.setIconClient("locationmanager.minimize",
                new ImageIcon(App.class.getResource("/data/bibliothek/gui/dock/common/icons/eclipse/minimize.png")));
        icons.setIconClient("locationmanager.externalize",
                new ImageIcon(App.class.getResource("/data/bibliothek/gui/dock/common/icons/eclipse/externalize.png")));
        icons.setIconClient(
                "locationmanager.unexternalize",
                new ImageIcon(App.class.getResource("/data/bibliothek/gui/dock/common/icons/eclipse/unexternalize.png")));
        icons.setIconClient("locationmanager.unmaximize_externalized",
                new ImageIcon(App.class.getResource("/data/bibliothek/gui/dock/common/icons/eclipse/normalize.png")));
        icons.setIconClient("flap.hold",
                new ImageIcon(App.class.getResource("/data/bibliothek/gui/dock/core/eclipse/holdon.png")));
        icons.setIconClient("flap.free",
                new ImageIcon(App.class.getResource("/data/bibliothek/gui/dock/core/eclipse/holdoff.png")));
        icons.setIconClient("close",
                new ImageIcon(App.class.getResource("/data/bibliothek/gui/dock/core/eclipse/closex.png")));
    }

    private static void initLookAndFeel() throws ClassNotFoundException, InstantiationException, IllegalAccessException {
        /**
         * try { UIManager.setLookAndFeel(DefaultUserContext.getInstance().getTheme()); } catch (UnsupportedLookAndFeelException e) {
         * LOG.warn(DefaultI18nContext.getInstance().i18n( "Unable to install the selected look and feel because it's unsupported.")); }
         **/

    }

    private static void initModules() {
        Map<String, BaseTaskExecutionModule> modules = ApplicationContextHolder.getContext().getBeansOfType(
                BaseTaskExecutionModule.class);
        LOG.debug("Found {} modules", modules.size());
        TaskExecutionModulesLoadedEvent initModulesEvent = new TaskExecutionModulesLoadedEvent();
        initModulesEvent.addAll(modules.values());
        EventBus.publish(initModulesEvent);

    }
}
